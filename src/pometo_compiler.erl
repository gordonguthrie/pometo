-module(pometo_compiler).

-export([
			compile/1
		]).

-include("parser_records.hrl").

compile({#liffey{} = L, Map}) when Map == #{} ->
	ModuleName = pometo,
	M0 = lfe_gen:new_module(ModuleName),
    M1 = lfe_gen:add_exports([[run,0]], M0),
    M2 = lfe_gen:add_form([defun, run, [], expand(L)], M1),
    Forms = lfe_gen:build_mod(M2),
    io:format("Forms is ~p~n", [Forms]),
    {ok, ModuleName, Binary, Warnings} = lfe_gen:compile_mod(M2),
    {module, ModuleName} = code:load_binary(ModuleName, "nofile", Binary),
    ok.

expand(#liffey{op = {Op, Decorator}, args = Args})  ->
	expand(#liffey{op = Op, args = [Decorator | Args]});
expand(#liffey{op = Op, args = Args}) ->
	io:format("Op is ~p Args is ~p~n", [Op, Args]),
	[':', pometo_runtime, Op, [list | Args] ++ ["bindings_in_here"]].


