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

% rho needs to know the length of the vector
% this clause handles both a scalar and a vector on either of the LHS or the RHS
dyadic_RUNTIME(["⍴", #'$ast¯'{op      = ?shape(N1, Type1) = Shp,
															args    = A1,
															line_no = LNo,
															char_no = CNo},
												 #'$ast¯'{op      = ?shape(N2, Type2),
																	args    = A2} = Right]) when Type1 == number  orelse
																	Type1 == boolean ->
	% this casts any scalars on either side to vectors
	NewN2 = case N2 of
			unsized_vector -> [length(A2)];
			_              -> N2
	end,

	NewA1 = case N1 of
				0 -> [A1];   % scalar
				_ -> A1
			end,
	NewA2 = case NewN2 of
				0 -> [A2];   % scalar
				_ -> A2
			end,
	case pometo_runtime:are_all_positive_integers(NewA1) of
		true ->
			{Size, NewA1} = case NewA1 of
				Map  when is_map(Map)   -> {pometo_runtime:get_no_of_elements_from_args(NewA1), NewA1};
				List when is_list(List) -> {pometo_runtime:get_no_of_elements_from_args(NewA1), NewA1};
				N                       -> {N, [N]}
			end,
			SliceSize = pometo_runtime:get_no_of_elements_from_dims(NewN2),
			NewArgs = if
						Size <  SliceSize -> pometo_runtime:snip_args(NewA2, Size);
						Size == SliceSize -> NewA2;
						Size >  SliceSize -> Repetitions = trunc(Size/SliceSize),
																 Rem = Size rem SliceSize,
																 TopUp = pometo_runtime:snip_args(NewA2, Rem),
																 pometo_runtime:extend(NewA2, SliceSize, Repetitions, TopUp, Rem)
						 end,
			Indexed = if
				is_map(NewArgs)  -> true;
				is_list(NewArgs) -> false
			end,
			Right#'$ast¯'{op   = Shp#'$shape¯'{dimensions = pometo_runtime:args_to_list(NewA1),
																				 indexed    = Indexed,
																				 type       = Type2},
										args = NewArgs};
		false ->
			Msg1 = "dyadic ⍴ only accepts integer arguments to the left and was called with",
			Args1 = pometo_runtime:args_to_list(A1),
			Args2 = pometo_runtime:args_to_list(A2),
			Msg2 = io_lib:format("Left: ~p - Right: ~p", [Args1, Args2]),
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
dyadic_RUNTIME([Op, #'$ast¯'{op = ?shp(0),
														 args = A1} = Left,
										#'$ast¯'{op = ?shp(0),
														 args = A2}]) ->
	Val = execute_dyadic(Op, A1, A2),
	Left#'$ast¯'{args = Val};
% if one side is a scalar cast it to an array
dyadic_RUNTIME([Op, #'$ast¯'{op = ?shp(0)}  = Left,
										#'$ast¯'{op = ?shp(_N)} = Right]) ->
	% order of A2 and A1 swapped and return record based on 2nd shp
	apply(Op, Right, make_vector(Left), left);
dyadic_RUNTIME([Op, #'$ast¯'{op = ?shp(_N)} = Left,
										#'$ast¯'{op = ?shp(0)}  = Right]) ->
	apply(Op, Left, make_vector(Right), right);
%% now plain number handling
%% this clause will match two unsized vectors or two vectors of the same size
dyadic_RUNTIME([Op, #'$ast¯'{op = ?shp(N)} = Left,
										#'$ast¯'{op = ?shp(N)} = Right]) ->
	zip(Op, Left, Right);
%% this clause will handle on mixed case
dyadic_RUNTIME([Op, #'$ast¯'{op = ?shp(unsized_vector)} = Left,
										#'$ast¯'{op = ?shp(L)}              = Right]) when is_list(L) ->
	zip(Op, Left, Right);
%% this clause will handle the other
dyadic_RUNTIME([Op, #'$ast¯'{op = ?shp(L)} = Left,
										#'$ast¯'{op = ?shp(unsized_vector)} = Right]) when is_list(L) ->
	zip(Op, Left, Right);
dyadic_RUNTIME([Op, #'$ast¯'{op = ?shp([1])} = Left,
										#'$ast¯'{}               = Right]) ->
	% order of A2 and A1 swapped and return record based on 2nd shp
	apply(Op, Right, Left, left);
dyadic_RUNTIME([Op, #'$ast¯'{}               = Left,
										#'$ast¯'{op = ?shp([1])} = Right]) ->
	apply(Op, Left, Right, right);
dyadic_RUNTIME([Op, #'$ast¯'{op      = ?shp(N1),
														 line_no = LNo,
														 char_no = CNo},
										#'$ast¯'{op      = ?shp(N2)}]) ->
	Lhs = case is_list(N1) of
		true  -> string:join([get_fmt(X) || X <- N1], ", ");
		false -> get_fmt(N1)
	end,
	Rhs = case is_list(N2) of
		true  -> string:join([get_fmt(X) || X <- N2], ", ");
		false -> get_fmt(N2)
	end,
	Msg1  = io_lib:format("dimensions mismatch in dyadic ~p", [Op]),
	Msg2  = io_lib:format("LHS dimensions ~p: RHS dimensions ~p", [Lhs, Rhs]),
	Error = pometo_runtime_format:make_error("LENGTH ERROR", Msg1, Msg2, LNo, CNo),
	throw({error, Error}).

%%
%% private fns
%%

make_vector(#'$ast¯'{op   = #'$shape¯'{dimensions = 0},
										 args = A} = AST) ->
	AST#'$ast¯'{op   = #'$shape¯'{dimensions = [1]},
							args = [A]}.

get_fmt(X) ->
	#fmt_segment{strings = [Str]} = pometo_runtime_format:fmt(X),
	Str.

make_ast(Arg) -> Shp = #'$shape¯'{dimensions = 0},
				 #'$ast¯'{op = Shp, args = Arg}.

do_complex(Op, A1, A2) when Op == "+" orelse
							Op == "-" ->
	% call zip with dummy line and char nos because we know it won't fail
	% and they are only needed for the error message
	% make 'em grepable because yeah-it-can-never-get-there code ***ALWAYS*** gets executed
	% that's the law, I didn't fuckin make it, ok? bud...
	{_NoArgs, Vals} = zip_LL(A1, A2, Op, -98765, -98765, ?START_COUNTING_ARGS, ?EMPTY_ACCUMULATOR),
	Vals;
do_complex(Op, [Rl1, Im1], [Rl2, Im2]) when Op == "×" ->
	[Rl1 * Rl2 - Im1 * Im2, Rl1 * Im2 + Im1 * Rl2];
do_complex(Op, [Rl1, Im1], [Rl2, Im2]) when Op == "÷" ->
	Sq = Rl2 * Rl2 + Im2 * Im2,
	Real = (Rl1 * Rl2 + Im1 * Im2)/Sq,
	Imag = (Im1 * Rl2 - Rl1 * Im2)/Sq,
	[Real, Imag].

%% eek 4 different zips (convert to three zip fns tho ;-)

% * List <-> List => zip_LL
% * Map  <-> Map  => zip_MM
% * Map  <-> List => zip_ML right
% * List <-> Map  => zip_ML left (flip arguments)

% Unindexed get zipped against Unindexed
% if the left list was lazy it ain't no longer
zip(Op, #'$ast¯'{op      = #'$shape¯'{dimensions = D,
																			indexed = false} = Shp,
								 args    = Args1,
								 line_no = LNo,
								 char_no = CNo} = AST1,
				#'$ast¯'{op      = #'$shape¯'{indexed = false},
								 args    = Args2}) ->
	{NoArgs, NewVals} = zip_LL(Args1, Args2, Op, LNo, CNo, ?START_COUNTING_ARGS, ?EMPTY_ACCUMULATOR),
	NewDims = case D of
		unsized_vector -> [NoArgs];
		_              -> D
	end,
	AST1#'$ast¯'{op   = Shp#'$shape¯'{dimensions = NewDims},
							 args = NewVals};
%% Indexed get zipped against Indexed
zip(Op, #'$ast¯'{op      = #'$shape¯'{indexed = true},
								 args    = Args1,
								 line_no = LNo,
								 char_no = CNo} = AST1,
				#'$ast¯'{op      = #'$shape¯'{indexed = true},
								 args    = Args2}) ->
	I1 = maps:iterator(Args1),
	I2 = maps:iterator(Args2),
	NewArgs = zip_MM(Args1, Args2, I1, I2, Op, LNo, CNo),
	AST1#'$ast¯'{args = NewArgs};
% an indexed versus an unindexed results in an indexed
zip(Op,	#'$ast¯'{op      = #'$shape¯'{indexed = true},
								 args    = Args1,
								 line_no = LNo,
								 char_no = CNo} = AST1,
				#'$ast¯'{op      = #'$shape¯'{indexed = false},
								 args    = Args2}) ->
	I1 = maps:iterator(Args1),
	NewArgs = zip_ML(Args1, Args2, I1, right, Op, ?START_COUNTING_ARGS, LNo, CNo),
	AST1#'$ast¯'{args = NewArgs};
% an unindexed versus an indexed results in an indexed
% to do that we flip the arguments and mark the zipping as left
zip(Op,	#'$ast¯'{op      = #'$shape¯'{indexed = false},
								 args    = Args1,
								 line_no = LNo,
								 char_no = CNo},
				#'$ast¯'{op      = #'$shape¯'{indexed = true},
								 args    = Args2} = AST2) ->
	I2 = maps:iterator(Args2),
	NewArgs = zip_ML(Args2, Args1, I2, left, Op, ?START_COUNTING_ARGS, LNo, CNo),
	AST2#'$ast¯'{args = NewArgs}.

% eek, peeking inside the map iterator, bad Gordon
zip_ML(Map1,  [],  				none,       _,    _,  _,      _,      _)      -> Map1;
zip_ML(_Map2, [_H2 | T2], {_, _, I1}, _Dir, Fn, LineNo, CharNo, N) when (I1 == none  andalso
																																				 T2 /= [])   orelse
																																				(I1 /= none  andalso
																																				 T2 == []) ->
	End = get_end(I1, N),
	Msg1 = io_lib:format("dimensions mismatch in dyadic ~p", [Fn]),
	Msg2 = case End of
		1 -> io_lib:format("ran out of matches after 1 element",   []);
		_ -> io_lib:format("ran out of matches after ~p elements", [End])
	end,
	Error = pometo_runtime_format:make_error("LENGTH ERROR", Msg1, Msg2, LineNo, CharNo),
	throw({error, Error});
zip_ML(Map1, [H2 | T2], I1, Dir, Fn, LineNo, CharNo, N) ->
	{Key1, Val1, NewI1} = maps:next(I1),
	NewVal = case Dir of
		right -> execute_dyadic(Fn, Val1, H2);
		left  -> execute_dyadic(Fn, H2, Val1)
	end,
	NewMap1 = maps:put(Key1, NewVal, Map1),
	zip_ML(NewMap1, T2, NewI1, Dir, Fn, LineNo, CharNo, N + 1).

zip_MM(Map1, _Map2, none, none,  _,  _,      _)      -> Map1;
zip_MM(Map2, Map2,  I1,   I2,    Fn, LineNo, CharNo) when (I1 == none  andalso
																													 I2 /= none) orelse
																													(I1 /= none  andalso
																													 I2 == none) ->
	End = get_end(I1, I2),
	Msg1 = io_lib:format("dimensions mismatch in dyadic ~p", [Fn]),
	Msg2 = case End of
		1 -> io_lib:format("ran out of matches after 1 element",   []);
		_ -> io_lib:format("ran out of matches after ~p elements", [End])
	end,
	Error = pometo_runtime_format:make_error("LENGTH ERROR", Msg1, Msg2, LineNo, CharNo),
	throw({error, Error});
zip_MM(Map1, Map2, I1, I2, Fn, LineNo, CharNo) ->
	{Key1,  Val1, NewI1} = maps:next(I1),
	{_Key2, Val2, NewI2} = maps:next(I2),
	NewVal  = execute_dyadic(Fn, Val1, Val2),
	NewMap1 = maps:put(Key1, NewVal, Map1),
	zip_MM(NewMap1, Map2, NewI1, NewI2, Fn, LineNo, CharNo).

get_end(none,   {K, _V}) -> K;
get_end({K, _V}, none)   -> K;
get_end(none,    N)      -> N;
get_end(N,       none)   -> N.

zip_LL([], [], _, _, _, N, Acc) -> {N, lists:reverse(Acc)};
zip_LL([_H1 | T1], [_H2 | T2], Fn, LineNo, CharNo, N, _Acc)	when (T1 == []  andalso
																																	T2 /= []) orelse
																																 (T1 /= []  andalso
																																	T2 == []) ->
	Msg1 = io_lib:format("dimensions mismatch in dyadic ~p", [Fn]),
	Msg2 = case N + 1 of
		1 -> io_lib:format("ran out of matches after 1 element",   []);
		_ -> io_lib:format("ran out of matches after ~p elements", [N + 1])
	end,
	Error = pometo_runtime_format:make_error("LENGTH ERROR", Msg1, Msg2, LineNo, CharNo),
	throw({error, Error});
zip_LL([H1 | T1], [H2 | T2], Fn, LineNo, CharNo, N, Acc) ->
	NewAcc = execute_dyadic(Fn, H1, H2),
	zip_LL(T1, T2, Fn, LineNo, CharNo, N + 1, [NewAcc | Acc]).

% we have two applies - one with a map and one with a zip

apply(Op, AST, Singleton, Direction) ->
	#'$ast¯'{args = Args} = AST,
	#'$ast¯'{args = Arg}  = Singleton,
	Val = get_val(Arg),
	case is_map(Args) of
			true  -> Iter = maps:iterator(Args),
							 NewArgs = applyM(Args, Val, Iter, Op, Direction),
							 AST#'$ast¯'{args = NewArgs};
			false -> {NoArgs, NewArgs} = applyL(Args, Val, Direction, Op, ?START_COUNTING_ARGS, ?EMPTY_ACCUMULATOR),
							 maybe_make_eager(AST#'$ast¯'{args = NewArgs}, NoArgs)
	end.

applyM(Map, _, none, _, _) -> Map;
applyM(Map, Val, Iter, Fn, Direction) ->
	{K, V, I} = maps:next(Iter),
	NewV = case Direction of
		left  -> execute_dyadic(Fn, Val, V);
		right -> execute_dyadic(Fn, V,   Val)
	end,
	NewMap = maps:put(K, NewV, Map),
	applyM(NewMap, Val, I, Fn, Direction).

applyL([], _, _Direction, _Fn, N, Acc) -> {N, lists:reverse(Acc)};
applyL([H | T], V, left, Fn, N, Acc) ->
	NewAcc = execute_dyadic(Fn, V, H),
	applyL(T, V, left, Fn, N + 1, [NewAcc | Acc]);
applyL([H | T], V, right, Fn, N, Acc) ->
	NewAcc = execute_dyadic(Fn, H, V),
	applyL(T, V, right, Fn, N + 1, [NewAcc | Acc]).

get_val([N])              -> N;
get_val(M) when is_map(M) -> {1, Val} = maps:get(1, M),
																				Val.

maybe_make_eager(#'$ast¯'{op = #'$shape¯'{dimensions = unsized_vector} = Shp} = AST, NoArgs) ->
	AST#'$ast¯'{op = Shp#'$shape¯'{dimensions = [NoArgs]}};
maybe_make_eager(X, _N) ->
	X.

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
