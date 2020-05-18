-module(pometo_runtime).

-include("parser_records.hrl").

-export([
		  rho/1,
		  run_ast/2,
		  format/1
		]).

-define(EMPTY_ACCUMULATOR, []).
-define(SPACE, 32).


run_ast(#let_op{vals = Vals} = Binding, Bindings) -> {Vals, [Binding | Bindings]};
run_ast(#expr{type        = scalar,
			  application = dyadic,
	          fn_name     = Fn,
			  args        = [
			  			     #'¯¯⍴¯¯'{dimensions = D, vals = V1},
			                 #'¯¯⍴¯¯'{dimensions = D, vals = V2}
			                ]}, Bindings) ->
	Shape = #'¯¯⍴¯¯'{dimensions = D, vals = zip(V1, V2, Fn, ?EMPTY_ACCUMULATOR)},
	{Shape, Bindings};
run_ast(#expr{type        = scalar,
			  application = dyadic,
	          fn_name     = Fn,
			  args        = [
			  			     #'¯¯⍴¯¯'{dimensions = [1], vals = [V1]},
			                 #'¯¯⍴¯¯'{dimensions = D,   vals = V2}
			                ]}, Bindings) ->
	Shape = #'¯¯⍴¯¯'{dimensions = D, vals = apply(V2, V1, left, Fn, ?EMPTY_ACCUMULATOR)},
	{Shape, Bindings};
run_ast(#expr{type        = scalar,
			  application = dyadic,
	          fn_name     = Fn,
			  args        = [
			  			     #'¯¯⍴¯¯'{dimensions = D,   vals = V1},
			                 #'¯¯⍴¯¯'{dimensions = [1], vals = [V2]}
			                ]}, Bindings) ->
	Shape = #'¯¯⍴¯¯'{dimensions = D, vals = apply(V1, V2, right, Fn, ?EMPTY_ACCUMULATOR)},
	{Shape, Bindings};
run_ast(#expr{type        = scalar,
			  application = monadic,
	          fn_name     = Fn,
			  args        = [
			                 #'¯¯⍴¯¯'{dimensions = D, vals = V}
			                ]}, Bindings) ->
	Shape =#'¯¯⍴¯¯'{dimensions = D, vals = [execute_monadic(Fn, X) || X <- V]},
	{Shape, Bindings}.

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

rho(List) when is_list(List) ->
	Len = length(List),
	#'¯¯⍴¯¯'{style      = eager,
	         indexed    = false,
	         dimensions = [Len],
	         vals       = List}.

format(	#'¯¯⍴¯¯'{vals = V}) ->
	string:join([fmt(X) || X <- V], [" "]).

fmt(X) when X < 0 -> io_lib:format("¯~p", [abs(X)]);
fmt(X)            -> io_lib:format("~p",  [X]).