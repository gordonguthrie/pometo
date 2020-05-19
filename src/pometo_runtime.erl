-module(pometo_runtime).

-include("parser_records.hrl").

-export([
		  rho/1,
		  run_ast/1,
		  format/1
		]).

-define(EMPTY_ACCUMULATOR, []).
-define(SPACE, 32).

run_ast({#liffey{op = #'¯¯⍴¯¯'{}} = L, Bindings}) ->
	{L, Bindings};
run_ast({#liffey{op = {dyadic, Op}, args = [A1, A2]}, Bindings}) ->
	dyadic(Op, A1, A2, Bindings);
run_ast({#liffey{op = {monadic, Op}, args = [A]}, Bindings}) ->
	monadic(Op, A, Bindings).

rho(List) when is_list(List) ->
	Len = length(List),
	#'¯¯⍴¯¯'{style      = eager,
	         indexed    = false,
	         dimensions = [Len]}.

dyadic(Op, #liffey{op = #'¯¯⍴¯¯'{dimensions = N}, args = A1} = L1,
	       #liffey{op = #'¯¯⍴¯¯'{dimensions = N}, args = A2}, Bindings) ->
		Vals = zip(A1, A2, Op, ?EMPTY_ACCUMULATOR),
		{L1#liffey{args = Vals}, Bindings};
dyadic(Op, #liffey{op = #'¯¯⍴¯¯'{dimensions = [1]}, args = [A1]},
	       #liffey{                                 args = A2} = L2, Bindings) ->
		% order of A2 and A1 swapped and return record based on 2nd rho
		Vals = apply(A2, A1, left, Op, ?EMPTY_ACCUMULATOR),
		{L2#liffey{args = Vals}, Bindings};
dyadic(Op, #liffey{                                 args = A1} = L1,
	       #liffey{op = #'¯¯⍴¯¯'{dimensions = [1]}, args = [A2]}, Bindings) ->
		Vals = apply(A1, A2, right, Op, ?EMPTY_ACCUMULATOR),
		{L1#liffey{args = Vals}, Bindings}.

monadic(Op, #liffey{args = A} = L, Bindings) ->
	NewA = [execute_monadic(Op, X) || X <- A],
	{L#liffey{args = NewA}, Bindings}.

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

execute_dyadic("+", L, R) -> L + R;
execute_dyadic("-", L, R) -> L - R;
execute_dyadic("×", L, R) -> L * R;
execute_dyadic("÷", L, R) -> L / R.

execute_monadic("+", V) -> V; % complex conjugate stub. return identity.
execute_monadic("-", V) -> -1 * V;
execute_monadic("×", V) -> signum(V); % when complex numbers are introduced, this becomes {⍵÷|⍵}.
execute_monadic("÷", V) -> 1 / V.

signum(V) when V < 0 ->
    -1;
signum(V) when V == 0 ->
    0;
signum(V) when V > 0 ->
    1.

format(#liffey{op = #'¯¯⍴¯¯'{}, args = Args}) ->
	lists:flatten(string:join([fmt(X) || X <- Args], [" "])).

fmt(X) when X < 0 -> io_lib:format("¯~p", [abs(X)]);
fmt(X)            -> io_lib:format("~p",  [X]).
