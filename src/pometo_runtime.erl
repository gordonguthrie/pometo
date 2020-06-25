-module(pometo_runtime).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").

%% things exported for runtime
-export([
		  run_ast/2,
		  are_all_integers/1,
		  are_all_positive_integers/1,
		  product/1
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
		throw:E -> io:format("Error thrown ~p~n", [E]),
				   {error, #error{} = Err} = E,
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

are_all_integers([])                         -> true;
are_all_integers([H | T]) when is_integer(H) -> are_all_integers(T);
are_all_integers(X)       when is_integer(X) -> true;
are_all_integers(_)                          -> false.

are_all_positive_integers([])                                 -> true;
are_all_positive_integers([H | T]) when is_integer(H)         -> are_all_positive_integers(T);
are_all_positive_integers(X)       when is_integer(X) andalso
                                        X >= 0                -> true;
are_all_positive_integers(_)                                  -> false.

%%
%% Exported for use in compiled modules
%%

product([H | T]) -> lists:foldl(fun(X, Acc) -> X * Acc end, H, T);
product(N)       -> N.

dyadic(Args)  -> pometo_runtime_dyadic:dyadic_RUNTIME(Args).

monadic(Args) -> pometo_runtime_monadic:monadic_RUNTIME(Args).

apply_fn([Mod, Fun, Arg]) -> Mod:Fun(Arg).

runtime_let([#'$ast¯'{op = runtime_let, args = Args}]) -> run_ast2(Args).
