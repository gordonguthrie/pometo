-module(pometo_compiler).

-export([
			compile/2,
			compile/1
		]).

-include("parser_records.hrl").
-include_lib("eunit/include/eunit.hrl").

%% fake compile point until we get full compile syntax
%% gotta call the damn module something in the meantime
compile(#liffey{} = L) -> compile(L, pometo).

compile(#liffey{} = L, ModuleName) ->
	M0 = lfe_gen:new_module(ModuleName),
    M1 = lfe_gen:add_exports([[run,0]], M0),
    M2 = lfe_gen:add_form([defun, run, [], expand(L)], M1),
    Forms = lfe_gen:build_mod(M2),
    {ok, ModuleName, Binary, _Warnings} = lfe_gen:compile_mod(M2),
    {module, ModuleName} = code:load_binary(ModuleName, "nofile", Binary).

expand(#liffey{op = {Op, Decorator}, args = Args})  ->
	expand(#liffey{op = Op, args = [Decorator | Args]});
expand(#liffey{op = #'¯¯⍴¯¯'{} = Rho, args = Args} = Liffey) ->
	NewRho = make_tuple(Rho),
	NewArgs = [list | [make_tuple(X) || X <- Args]],
	liffey_to_tuple(#liffey{op = NewRho, args = NewArgs});
expand(#liffey{op = 'let', args = [Var, Arg | []]}) ->
	ExpArg = expand(Arg),
	['let', [[Var, ExpArg]], Var];
expand(#liffey{op = Op, args = Args}) ->
	ExpArgs = [expand(A) || A <- Args],
	[':', pometo_runtime, Op, [list | ExpArgs]];
expand(X) -> X.

make_tuple(T) when is_tuple(T) -> NewTuple = tuple_to_list(T),
                                  [tuple | [maybe_quote(X) || X <- NewTuple]];
make_tuple(X)                  -> X.

maybe_quote(A) when is_atom(A)  -> [quote, A];
maybe_quote(T) when is_tuple(T) -> make_tuple(T);
maybe_quote(X)                  -> X.

liffey_to_tuple(#liffey{} = L) ->
	[H | T] = tuple_to_list(L),
	[tuple, [quote, H] | T].
