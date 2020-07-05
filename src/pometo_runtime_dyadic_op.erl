-module(pometo_runtime_dyadic_op).

-export([
					dyadic_op_RUNTIME/1
				]).

%% exported so it can be passed in a function call to another module
-export([
					reduce_zip/3
				]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").
-include("runtime_include.hrl").

dyadic_op_RUNTIME([{"/", Rank},
										#'$ast¯'{do      = ?shape(D1, Type),
														 line_no = LNo,
														 char_no = CNo}      = AST1,
										#'$ast¯'{do      = ?shp(D2)} = AST2]) when Type == number  orelse
																															 Type == boolean orelse
																															 Type == mixed   orelse
																															 Type == complex orelse
																															 Type == array ->
	io:format("in dyadic_op_RUNTIME with ~p~n- AST1 ~p~n- AST2 ~p~n", [Rank, AST1, AST2]),
	NewAst1 = pometo_runtime:maybe_make_vector(AST1),
	NewAst2 = pometo_runtime:maybe_make_vector(AST2),
	Ranking = pometo_runtime:check_rank(D1, D2),
	io:format("checked D1 ~p and D2 ~p got ~p~n", [D1, D2, Ranking]),
	case Ranking of
		identical ->
			case length(D1) of
				1 -> pometo_runtime_dyadic:zip("/", NewAst1, NewAst2, fun reduce_zip/3);
				_ -> make_reduce_error(D1, LNo, CNo)
			end;
		unsized_vector ->
			pometo_runtime_dyadic:zip("/", NewAst1, NewAst2, fun reduce_zip/3);
		_ -> rubette
	end.

%%
%% Private Fns
%%

reduce_zip(N, Vals, Acc) when is_list(Vals) andalso
															is_list(Acc)         -> Len = length(Vals),
																											{N + Len, Vals ++ Acc};
reduce_zip(N, Val,  Acc) when is_list(Acc)         -> {N + 1, [Val | Acc]};
reduce_zip(N, Vals, Map) when is_list(Vals) andalso
															is_map(Map)          -> AccFun = fun(V, {K, M}) ->
																								       {K + 1, maps:put(K, V, M)}
																							        end,
																							        lists:foldl(AccFun, {N, Map}, Vals);
reduce_zip(N, Val,  Map) when is_map(Map)          -> {N + 1, maps:put(N, Val, Map)}.

make_reduce_error(_A, _B, _C) -> borquelmismo.