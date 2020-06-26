-module(pometo_runtime).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").

%% things exported for runtime
-export([
		  run_ast/2,
		  are_all_integers/1,
		  are_all_positive_integers/1,
		  product/1,
		  index/1,
		  make_indexed/1,
		  unindex/1,
		  force_index/2
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
				  args = [Arg]})              -> apply_fn([Mod, Fn, Arg]);
run_ast2(#'$ast¯'{op   = #'$shape¯'{}} = AST) -> AST;
run_ast2(#'$ast¯'{op   = 'let',
				  args = [_V, A | []]} = AST) -> NewL = AST#'$ast¯'{op   = runtime_let,
																	args = A},
												 runtime_let([NewL]);
run_ast2(#'$ast¯'{op   = {dyadic, Op},
				  args = [A1, A2]})           -> dyadic([Op, A1, A2]);
run_ast2(#'$ast¯'{op   = {monadic, Op},
				  args = [A]})                -> monadic([Op, A]);
run_ast2(#'$ast¯'{op   = flatten_comma,
				  args = [A]})                -> #'$ast¯'{op   = #'$shape¯'{} = R,
														  args = InnerA}      = A,
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

product([H | T]) -> lists:foldl(fun(X, Acc) -> X * Acc end, H, T);
product(N)       -> N.

index(Args) ->
	IndexFn = fun(X, {N, Map}) ->
		NewMap = maps:put(N, X, Map),
		{N + 1, NewMap}
	end,
	lists:foldl(IndexFn, {1, #{}}, Args).

% Maps have a list order when all keys are indexes
unindex(Args) when is_map(Args) -> List = maps:to_list(Args),
								   {_Keys, Vals} = lists:unzip(List),
								   Vals.

% don't do anything scalars
force_index(#'$ast¯'{op   = #'$shape¯'{dimensions = 0}} = AST, _) ->
	AST;
% do work on unindexed arrays
force_index(#'$ast¯'{op   = #'$shape¯'{} = Shp} = AST, Forcing) when Forcing == index  orelse
																	 Forcing == unindex ->
	Ret = AST#'$ast¯'{op = Shp#'$shape¯'{forcing = Forcing}},
				io:format("AST is ~p~n Ret is ~p~n", [AST, Ret]),
					Ret;
% don't do anything to anything else
force_index(X, _) ->
	X.

% don't do anything scalars
make_indexed(#'$ast¯'{op   = #'$shape¯'{dimensions = 0}} = AST) ->
	AST;
% do work on unindexed arrays
make_indexed(#'$ast¯'{op   = #'$shape¯'{indexed = false} = Shp,
					  args = Args} = AST) ->
	{_Len, NewArgs} = pometo_runtime:index(Args),
	AST#'$ast¯'{op   = Shp#'$shape¯'{indexed = true},
				args = NewArgs};
% don't do anything to anything else
make_indexed(X) ->
	X.

%%
%% Exported for use in compiled modules
%%

dyadic(Args)  -> pometo_runtime_dyadic:dyadic_RUNTIME(Args).

monadic(Args) -> pometo_runtime_monadic:monadic_RUNTIME(Args).

apply_fn([Mod, Fun, Arg]) -> Mod:Fun(Arg).

runtime_let([#'$ast¯'{op = runtime_let, args = Args}]) -> run_ast2(Args).
