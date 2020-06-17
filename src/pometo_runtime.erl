-module(pometo_runtime).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").

%% things exported for runtime
-export([
		  run_ast/2,
		  format/1,
		  format_errors/1
		]).

%% exported for inclusion in compiled modules
%% should never be called directly
-export([
		dyadic/1,
		monadic/1,
		runtime_let/1
		]).

-define(EMPTY_ACCUMULATOR, []).
-define(SPACE, 32).

-define(rho(Dim), #'$¯¯⍴¯¯'{dimensions = Dim}).

%%
%% Runtime API
%%

run_ast(AST, Str) ->
	try run_ast2(AST)
	catch
		throw:E -> {error, #error{} = Err} = E,
		           {error, Err#error{expr = Str}}
	end.

run_ast2(#ast{op   = #'$¯¯⍴¯¯'{}} = L)  -> L;
run_ast2(#ast{op   = 'let',
	          args = [_V, A | []]} = L) -> NewL = L#ast{op   = runtime_let,
                                                        args = A},
                                           runtime_let([NewL]);
run_ast2(#ast{op   = {dyadic, Op},
	          args = [A1, A2]})         -> dyadic([Op, A1, A2]);
run_ast2(#ast{op   = {monadic, Op},
	          args = [A]})              -> monadic([Op, A]);
run_ast2(#ast{op   = flatten_comma,
	          args = [A]})              -> #ast{op   = #'$¯¯⍴¯¯'{} = R,
												args = InnerA}     = A,
										   NewR = R#'$¯¯⍴¯¯'{dimensions = length(InnerA)},
										   A#ast{op = NewR}.

format([]) -> [];
format(List) when is_list(List) ->
	print_line(List);
%% this is fucked because the final element is the head of the list...
format(#ast{op = #'$¯¯⍴¯¯'{dimensions = D},
	        args = Args}) ->
	case length(D) of
		1 -> print_line(Args);
		2 -> "banjo"
	end;
%	build_print(lists:reverse(D), Args);
format({error, Err}) ->
	format_errors([Err]).

format_errors(Errors) ->
	FormattedEs = [format_error(X) || X <- Errors],
	lists:flatten(string:join(FormattedEs, "\n") ++ "\n").

%%
%% Exported for use in compiled modules
%%

runtime_let([#ast{op = runtime_let, args = Args}]) -> Args.

%% complex number handling first
%% some ops on complex numbers are simple scalar extentions
dyadic([Op, #ast{op = complex, args = A1} = L1,
	        #ast{op = complex, args = A2}]) when Op == "+" orelse
                                                 Op == "-" ->
		Vals = zip(A1, A2, Op, ?EMPTY_ACCUMULATOR),
		L1#ast{args = Vals};
dyadic([Op, #ast{op = complex, args = [Rl1, Im1]} = L1,
	        #ast{op = complex, args = [Rl2, Im2]}]) when Op == "×" ->
		L1#ast{args = [Rl1 * Rl2 - Im1 * Im2, Rl1 * Im2 + Im1 * Rl2]};
%% some are not
dyadic([Op, #ast{op = complex, args = [Rl1, Im1]} = L1,
	        #ast{op = complex, args = [Rl2, Im2]}]) when Op == "÷" ->
		Sq = Rl2 * Rl2 + Im2 * Im2,
		Real = (Rl1 * Rl2 + Im1 * Im2)/Sq,
		Imag = (Im1 * Rl2 - Rl1 * Im2)/Sq,
		L1#ast{args = [Real, Imag]};
dyadic([Op, #ast{op = complex,   args = [Real, Imag]} = L1,
	        #ast{op = ?rho([1]), args = A2}]) when Op == "+" orelse
                                                   Op == "-" ->
		[Val] = apply([Real], A2, right, Op, ?EMPTY_ACCUMULATOR),
		L1#ast{args = [Val, Imag]};
dyadic([Op, #ast{op = complex,   args = A1} = L1,
	        #ast{op = ?rho([1]), args = A2}]) ->
		Vals = apply(A1, A2, right, Op, ?EMPTY_ACCUMULATOR),
		L1#ast{args = Vals};
dyadic([Op, #ast{op = ?rho([1]), args = A1},
	        #ast{op = complex,   args = [Real, Imag]} = L2]) when Op == "+" orelse
                                                                  Op == "-"->
		[Val] = apply([Real], A1, left, Op, ?EMPTY_ACCUMULATOR),
		L2#ast{args = [Val, Imag]};
dyadic([Op, #ast{op = ?rho([1]), args = A1},
	        #ast{op = complex,   args = A2} = L2]) ->
		Vals = apply(A2, A1, left, Op, ?EMPTY_ACCUMULATOR),
		L2#ast{args = Vals};
%% now plain number handling
dyadic([Op, #ast{op = ?rho(N), args = A1} = L1,
	        #ast{op = ?rho(N), args = A2}]) ->
		Vals = zip(A1, A2, Op, ?EMPTY_ACCUMULATOR),
		L1#ast{args = Vals};
dyadic([Op, #ast{op = ?rho([1]), args = [A1]},
	        #ast{                args = A2} = L2]) ->
		% order of A2 and A1 swapped and return record based on 2nd rho
		Vals = apply(A2, A1, left, Op, ?EMPTY_ACCUMULATOR),
		L2#ast{args = Vals};
dyadic([Op, #ast{                args = A1} = L1,
	        #ast{op = ?rho([1]), args = [A2]}]) ->
		Vals = apply(A1, A2, right, Op, ?EMPTY_ACCUMULATOR),
		L1#ast{args = Vals};
dyadic([Op, #ast{op = ?rho(N1), line_no = LNo, char_no = ChNo},
	        #ast{op = ?rho(N2)}]) ->
		Msg1 = io_lib:format("dimensions mismatch in dyadic ~p", [Op]),
		Msg2 = io_lib:format("LHS dimensions ~p: RHS dimensions ~p", [N1, N2]),
		Error = #error{
		                type = "LENGTH ERROR",
						msg1 = Msg1,
						msg2 = Msg2,
						expr = "",
						at_line = LNo,
						at_char = ChNo
					   },
		throw({error, Error}).

monadic([Op, #ast{args = A} = L]) ->
	NewA = [execute_monadic(Op, X) || X <- A],
	L#ast{args = NewA}.

%%
%% Helper functions
%%

print_line(Args) ->
	RawLine = lists:flatten(string:join([fmt(X) || X <- Args], [" "])),
	if
		length(RawLine) > 74 -> {Line, _Discard} = lists:split(71, RawLine),
								Line ++ "...";
		el/=se               -> RawLine
	end.

% build_print(Dims, Args) ->
%	build_p(Dims, hd(Dims), Args, ?EMPTY_ACCUMULATOR)

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

execute_dyadic(Op,  #ast{} = L, #ast{} = R) -> dyadic([Op, L,         R]);
execute_dyadic(Op,  #ast{} = L, R)          -> dyadic([Op, L,         make_ast(R)]);
execute_dyadic(Op,  L,          #ast{} = R) -> dyadic([Op, make_ast(L), R]);
execute_dyadic("+", L, R) -> L + R;
execute_dyadic("-", L, R) -> L - R;
execute_dyadic("×", L, R) -> L * R;
execute_dyadic("÷", L, R) -> divide(L,R);
execute_dyadic("|", 0, R) -> R;
execute_dyadic("|", L, R) -> R/L.

%% complex nos first
execute_monadic("+", #ast{op   = complex,
                          args = [R, I]} = A) -> A#ast{args = [ R, -I]};
execute_monadic("-", #ast{op   = complex,
                          args = [R, I]} = A) -> A#ast{args = [-R, -I]};
execute_monadic("×", #ast{op   = complex,
                          args = [R, I]} = A) -> Mag = math:sqrt(R * R + I * I),
												 A#ast{args = unit_tensor(R,I,Mag)};
execute_monadic("÷", #ast{op   = complex,
                          args = [R, I]} = A) -> Sq = R * R + I * I,
												 A#ast{args = [R/Sq, -I/Sq]};

%% then plain ones
execute_monadic("+", V) -> V;
execute_monadic("-", V) -> -1 * V;
execute_monadic("×", V) -> signum(V);
execute_monadic("÷", V) -> 1 / V;
execute_monadic("|", V) -> abs(V).

% Scalar Functions
% 0÷0 ↔ 1
divide(L,R) when L == 0, R == 0 -> 1;
divide(L,R) -> L / R.

% ×0J0 ↔ 0J0
unit_tensor(_R,_I,Mag) when Mag == 0 -> [0,0];
unit_tensor( R, I,Mag)               -> [R / Mag, I / Mag].

signum(V) when V <  0 -> -1;
signum(V) when V == 0 -> 0;
signum(V) when V >  0 -> 1.

fmt(#ast{op = complex,
	     args = [R, I]})   -> fmt(R) ++ "J" ++fmt(I);
fmt(#ast{} = A)            -> " " ++ format(A);
fmt(X)          when X < 0 -> io_lib:format("¯~p", [abs(X)]);
fmt(X)                     -> io_lib:format("~p",  [X]).

make_ast(Arg) -> Rho = #'$¯¯⍴¯¯'{dimensions = [1]},
                 #ast{op = Rho, args = Arg}.

format_error(#error{type    = T,
					msg1    = M1,
					msg2    = M2,
					expr    = E,
					at_line = AtL,
					at_char = AtC}) ->
	Pointer = case AtC of
		999999 -> "";
		none   -> "";
		_      -> lists:flatten(lists:duplicate(AtC - 1, "-") ++ "^")
	end,
	io_lib:format("Error~n~ts~n~s~n~s (~s:~ts) on line ~p at character ~p~n",
				  [E, Pointer, T, M1, M2, AtL, AtC]).
