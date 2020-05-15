-module(pometo_runtime).

-include("parser_records.hrl").

-export([
		  rho/1,
		  run_ast/2,
		  format/1
		]).

-define(EMPTY_ACCUMULATOR, []).
-define(SPACE, 32).


run_ast(#expr{type        = scalar,
			  application = dyadic,
	          fn_name     = Fn,
			  args        = [
			  			     #'__⍴__'{dimensions = D, vals = V1},
			                 #'__⍴__'{dimensions = D, vals = V2}
			                ]}, _Dict) ->
	#'__⍴__'{dimensions = D, vals = zip(V1, V2, Fn, ?EMPTY_ACCUMULATOR)};
run_ast(#expr{type        = scalar,
			  application = dyadic,
	          fn_name     = Fn,
			  args        = [
			  			     #'__⍴__'{dimensions = [1], vals = [V1]},
			                 #'__⍴__'{dimensions = D,   vals = V2}
			                ]}, _Dict) ->
	#'__⍴__'{dimensions = D, vals = apply(V2, V1, left, Fn, ?EMPTY_ACCUMULATOR)};
run_ast(#expr{type        = scalar,
			  application = dyadic,
	          fn_name     = Fn,
			  args        = [
			  			     #'__⍴__'{dimensions = D,   vals = V1},
			                 #'__⍴__'{dimensions = [1], vals = [V2]}
			                ]}, _Dict) ->
	#'__⍴__'{dimensions = D, vals = apply(V1, V2, right, Fn, ?EMPTY_ACCUMULATOR)}.
% run_ast(#expr{type        = scalar,
%			  application = monadic,
%	          fn_name     = Fn,
%			  args        = [
%			                 #'__⍴__'{dimensions = D, vals = V}
%			                ]}, _Dict) ->
%	#'__⍴__'{dimensions = D, vals = [execute_monadic(Fn, X) || X <- V]}.


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
execute_dyadic("¯", L, R) -> L - R;
execute_dyadic("×", L, R) -> L * R;
execute_dyadic("÷", L, R) -> L / R.

% execute_monadic(_, V) -> V.

rho(List) when is_list(List) ->
	Len = length(List),
	#'__⍴__'{style      = eager,
	         indexed    = false,
	         dimensions = [Len],
	         vals       = List}.

format(	#'__⍴__'{vals = V}) -> string:join([io_lib:format("~p", [X]) || X <- V], [" "]).
