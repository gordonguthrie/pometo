-module(pometo_runtime_dyadic).

%% exported for inclusion in compiled modules
%% should never be called directly
-export([
			dyadic_RUNTIME/1
		]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").
-include("runtime_include.hrl").

% this clause handles both a scalar and a vector on eigher of the LHS or the RHS
dyadic_RUNTIME(["⍴", #'$ast¯'{op      = ?shape(N1, Type1) = Shp,
	                          args    = A1,
	                          line_no = LNo,
	                          char_no = CNo},
	                 #'$ast¯'{op      = ?shape(N2, Type2),
	                          args    = A2} = Right]) when Type1 == number  orelse
														   Type1 == boolean ->
	% this casts any LHS scalars on either side to vectors
	NewA1 = case N1 of
				0 -> [A1];
				_ -> A1
			end,
	NewA2 = case N2 of
				0 -> [A2];
				_ -> A2
			end,
	case pometo_runtime:are_all_integers(NewA1) of
		true ->
			{Size, NewA1} = case NewA1 of
				[H | T] -> {lists:foldl(fun(X, Acc) -> X * Acc end, H, T), NewA1};
				N       -> {N, [N]}
			end,
			SliceSize = length(NewA2),
			NewArgs = if
						Size <  SliceSize -> {Keep, _Discard} = lists:split(Size, NewA2),
											 Keep;
						Size == SliceSize -> NewA2;
						Size >  SliceSize -> Repetitions = trunc(Size/SliceSize),
											 Rem = Size rem SliceSize,
											 {TopUp, _Discard2} = lists:split(Rem, NewA2),
									 		 lists:flatten(lists:duplicate(Repetitions, NewA2)) ++ TopUp
					   end,
			Right#'$ast¯'{op   = Shp?shape(NewA1, Type2),
						  args = NewArgs};
		false ->
			Msg1 = "dyadic ⍴ only accepts integer arguments to the left and was called with",
			Msg2 = io_lib:format("Left: ~p - Right: ~p", [A1, A2]),
			Error = pometo_runtime_format:make_error("DOMAIN ERROR", Msg1, Msg2, LNo, CNo),
			throw({error, Error})
	end;
%% complex element number handling first
%% complex numbers in arrays are unpacked in execute_dyadic
%% some ops on complex numbers are simple scalar extentions
dyadic_RUNTIME([Op, ?complex_el(A1)   = Left,
	                ?complex_el(A2)]) ->
	Vals = do_complex(Op, A1, A2),
	Left?complex_el(Vals);
% mixed real and complex addition and subtraction
dyadic_RUNTIME([Op, ?complex_el(A1)       = Left,
	                #'$ast¯'{op = ?shp(0),
	                         args = A2}]) ->
	Vals = do_complex(Op, A1, [A2, 0]),
	Left?complex_el(Vals);
dyadic_RUNTIME([Op, #'$ast¯'{op   = ?shp(0),
	                         args = A1},
	                ?complex_el(A2) = Right]) ->
	Vals = do_complex(Op, [A1, 0], A2),
	Right?complex_el(Vals);

% handle scalars
% if both sides are scalar return a scalar
dyadic_RUNTIME([Op, #'$ast¯'{op = ?shp(0), args = A1} = Left,
	        #'$ast¯'{op = ?shp(0),
	                 args = A2}]) ->
	Val = execute_dyadic(Op, A1, A2),
	Left#'$ast¯'{args = Val};
% if one side is a scalar cast it to an array
dyadic_RUNTIME([Op, #'$ast¯'{op   = ?shp(0),
	                         args = A1},
	                #'$ast¯'{op   = ?shp(_N),
	                         args = A2}       = Right]) ->
	% order of A2 and A1 swapped and return record based on 2nd shp
	Vals = apply(A2, A1, left, Op, ?EMPTY_ACCUMULATOR),
	Right#'$ast¯'{args = Vals};
dyadic_RUNTIME([Op, #'$ast¯'{op   = ?shp(_N),
					         args = A1}     = Left,
	                #'$ast¯'{op   = ?shp(0),
	                         args = A2}])   ->
	Vals = apply(A1, A2, right, Op, ?EMPTY_ACCUMULATOR),
	Left#'$ast¯'{args = Vals};
%% now plain number handling
dyadic_RUNTIME([Op, #'$ast¯'{op   = ?shp(N),
							 args = A1} = Left,
	                #'$ast¯'{op   = ?shp(N),
	                		 args = A2}]) ->
	Vals = zip(A1, A2, Op, ?EMPTY_ACCUMULATOR),
	Left#'$ast¯'{args = Vals};
dyadic_RUNTIME([Op, #'$ast¯'{op   = ?shp([1]),
							 args = [A1]},
	                #'$ast¯'{args = A2} = Right]) ->
	% order of A2 and A1 swapped and return record based on 2nd shp
	Vals = apply(A2, A1, left, Op, ?EMPTY_ACCUMULATOR),
	Right#'$ast¯'{args = Vals};
dyadic_RUNTIME([Op, #'$ast¯'{args = A1} = Left,
	                #'$ast¯'{op   = ?shp([1]),
	                         args = [A2]}]) ->
	Vals = apply(A1, A2, right, Op, ?EMPTY_ACCUMULATOR),
	Left#'$ast¯'{args = Vals};
dyadic_RUNTIME([Op, #'$ast¯'{op      = ?shp(N1),
	                         line_no = LNo,
	                         char_no = CNo},
	                #'$ast¯'{op = ?shp(N2)}]) ->
	Lhs = case is_list(N1) of
		true  -> string:join([get_fmt(X) || X <- N1], ", ");
		false -> get_fmt(N1)
	end,
	Rhs = case is_list(N2) of
		true  -> string:join([get_fmt(X) || X <- N2], ", ");
		false -> get_fmt(N2)
	end,
	Msg1 = io_lib:format("dimensions mismatch in dyadic ~p", [Op]),
	Msg2 = io_lib:format("LHS dimensions ~p: RHS dimensions ~p", [Lhs, Rhs]),
	Error = pometo_runtime_format:make_error("LENGTH ERROR", Msg1, Msg2, LNo, CNo),
	throw({error, Error}).

%%
%% private fns
%%

get_fmt(X) ->
	#fmt_segment{strings = [Str]} = pometo_runtime_format:fmt(X),
	Str.

make_ast(Arg) -> Shp = #'$shape¯'{dimensions = 0},
                 #'$ast¯'{op = Shp, args = Arg}.

do_complex(Op, A1, A2) when Op == "+" orelse
							Op == "-" ->
	zip(A1, A2, Op, ?EMPTY_ACCUMULATOR);
do_complex(Op, [Rl1, Im1], [Rl2, Im2]) when Op == "×" ->
	[Rl1 * Rl2 - Im1 * Im2, Rl1 * Im2 + Im1 * Rl2];
do_complex(Op, [Rl1, Im1], [Rl2, Im2]) when Op == "÷" ->
	Sq = Rl2 * Rl2 + Im2 * Im2,
	Real = (Rl1 * Rl2 + Im1 * Im2)/Sq,
	Imag = (Im1 * Rl2 - Rl1 * Im2)/Sq,
	[Real, Imag].

	zip([], [], _, Acc) -> lists:reverse(Acc);
zip([H1 | T1], [H2 | T2], Fn, Acc) ->
	NewAcc = execute_dyadic(Fn, H1, H2),
	zip(T1, T2, Fn, [NewAcc | Acc]).

apply([], _, _Direction, _Fn, Acc) -> lists:reverse(Acc);
apply([H | T], V, left, Fn, Acc) ->
	NewAcc = execute_dyadic(Fn, V, H),
	apply(T, V, left, Fn, [NewAcc | Acc]);
apply([H | T], V, right, Fn, Acc) ->
	NewAcc = execute_dyadic(Fn, H, V),
	apply(T, V, right, Fn, [NewAcc | Acc]).


% first capture the complex nos
execute_dyadic(Op, ?complex_no(L), ?complex_no(R)) -> ?complex_no(do_complex(Op, L,      R));
execute_dyadic(Op, ?complex_no(L), R)              -> ?complex_no(do_complex(Op, L,      [R, 0]));
execute_dyadic(Op,  L,             ?complex_no(R)) -> ?complex_no(do_complex(Op, [L, 0], R));

%% complex arrays
execute_dyadic(Op,  #'$ast¯'{} = L, #'$ast¯'{} = R) -> dyadic_RUNTIME([Op, L,           R]);
execute_dyadic(Op,  #'$ast¯'{} = L, R)              -> dyadic_RUNTIME([Op, L,           make_ast(R)]);
execute_dyadic(Op,  L,              #'$ast¯'{} = R) -> dyadic_RUNTIME([Op, make_ast(L), R]);

execute_dyadic("+", L, R) -> L + R;
execute_dyadic("-", L, R) -> L - R;
execute_dyadic("×", L, R) -> L * R;
execute_dyadic("÷", L, R) -> L / R;
execute_dyadic("|", 0, R) -> R;
execute_dyadic("|", L, R) -> R/L.
