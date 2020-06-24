-module(pometo_runtime).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").

%% things exported for runtime
-export([
		  run_ast/2
		]).

%% exported for inclusion in compiled modules
%% should never be called directly
-export([
		runtime_let/1,
		apply_fn/1,
		dyadic/1,
		monadic/1
		]).

%%
%% Runtime API
%%

run_ast(AST, Str) ->
	try run_ast2(AST)
	catch
		throw:E -> {error, #error{} = Err} = E,
		           {error, Err#error{expr = Str}}
	end.

run_ast2(#'$ast¯'{op   = {apply_fn, {Mod, Fn}},
	              args = [Arg]})             -> apply_fn([Mod, Fn, Arg]);
run_ast2(#'$ast¯'{op   = #'$shape¯'{}} = L)  -> L;
run_ast2(#'$ast¯'{op   = 'let',
	              args = [_V, A | []]} = L) -> NewL = L#'$ast¯'{op   = runtime_let,
                                                                args = A},
                                               runtime_let([NewL]);
run_ast2(#'$ast¯'{op   = {dyadic, Op},
	              args = [A1, A2]})         -> dyadic([Op, A1, A2]);
run_ast2(#'$ast¯'{op   = {monadic, Op},
	              args = [A]})              -> monadic([Op, A]);
run_ast2(#'$ast¯'{op   = flatten_comma,
	              args = [A]})              -> #'$ast¯'{op   = #'$shape¯'{} = R,
												        args = InnerA}     = A,
										       NewR = R#'$shape¯'{dimensions = length(InnerA)},
										       A#'$ast¯'{op = NewR}.

%%
%% Exported for use in compiled modules
%%

dyadic(Args)  -> io:format("calling dyadic with ~p~n", [Args]),
				 pometo_runtime_dyadic:dyadic_RUNTIME(Args).

monadic(Args) -> pometo_runtime_monadic:monadic_RUNTIME(Args).

apply_fn([Mod, Fun, Arg]) -> Mod:Fun(Arg).

runtime_let([#'$ast¯'{op = runtime_let, args = Args}]) -> Args.
