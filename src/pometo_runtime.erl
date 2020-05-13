-module(pometo_runtime).

-include("parser_records.hrl").

-export([
		  rho/1,
		  run_ast/2,
		  execute/3
		]).

run_ast(#expr{type    = scalar,
	          fn_name = Fn,
			  args    = [
			  			  #rho{dimensions = D, vals = V1},
			              #rho{dimensions = D, vals = V2}
			            ]}, _Dict) ->
	zip(V1, V2, Fn, []).

zip([], [], _, Acc) -> lists:reverse(Acc);
zip([H1 | T1], [H2 | T2], Fn, Acc) ->
	NewAcc = execute(Fn, H1, H2),
	zip(T1, T2, Fn, [NewAcc | Acc]).

execute("+", L, R) -> L + R;
execute("¯", L, R) -> L - R;
execute("×", L, R) -> L * R;
execute("÷", L, R) -> L / R.

rho(List) when is_list(List) ->
	Len = length(List),
	#rho{style      = eager,
	     indexed    = false,
	     dimensions = [Len],
	     vals       = List}.