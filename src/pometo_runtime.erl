-module(pometo_runtime).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").
-include("runtime_include.hrl").

%% things exported for runtime
-export([
					run_ast/2,
					are_all_positive_integers/1,
					product/1,
					index/1,
					make_indexed/1,
					unindex/1,
					force_index/2,
					args_reverse/1,
					get_no_of_elements_from_args/1,
					get_no_of_elements_from_dims/1,
					make_dimensions/1,
					args_to_list/1,
					snip_args/2,
					extend/5,
					check_rank/2,
					maybe_make_vector/1,
					choose_accumulator/2,
					set_return_type/2,
					maybe_reverse/1
				]).

%% exported for inclusion in compiled modules
%% should never be called directly
-export([
					runtime_let/1,
					apply_fn/1,
					dyadic_fn/1,
					monadic_fn/1,
					dyadic_op/1,
					monadic_op/1
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

run_ast2(#'$ast¯'{do   = {apply_fn, {Mod, Fn}},
									args = [Arg]})              -> apply_fn([Mod, Fn, Arg]);
run_ast2(#'$ast¯'{do   = #'$shape¯'{}} = AST) -> AST;
run_ast2(#'$ast¯'{do   = 'let',
					args = [_V, A | []]} = AST) 				-> NewL = AST#'$ast¯'{do   = runtime_let,
																																		args = A},
																								 runtime_let([NewL]);
run_ast2(#'$ast¯'{do   = {dyadic_op, Do},
									args = [A1, A2]})           -> dyadic_op([Do, A1, A2]);
run_ast2(#'$ast¯'{do   = {monadic_op, Do},
									args = [A]})                -> monadic_op([Do, A]);
run_ast2(#'$ast¯'{do   = {dyadic_fn, Do},
									args = [A1, A2]})           -> dyadic_fn([Do, A1, A2]);
run_ast2(#'$ast¯'{do   = {monadic_fn, Do},
									args = [A]})                -> monadic_fn([Do, A]);
run_ast2(#'$ast¯'{do   = flatten_comma,
									args = [A]})                -> #'$ast¯'{do   = #'$shape¯'{} = R,
																													args = InnerA}      = A,
																								 NewR = R#'$shape¯'{dimensions = length(InnerA)},
																								 A#'$ast¯'{do = NewR}.

are_all_positive_integers([])                                 -> true;
are_all_positive_integers([H | T]) when is_integer(H)         -> are_all_positive_integers(T);
are_all_positive_integers(X)       when is_integer(X) andalso
																				X >= 0                -> true;
are_all_positive_integers(M)       when is_map(M)             -> I = maps:iterator(M),
																																 are_all_positive_ints2(I);
are_all_positive_integers(_)                                  -> false.

are_all_positive_ints2(none) -> true;
are_all_positive_ints2(I)    ->
	{_K, V, NewI} = maps:next(I),
	case are_all_positive_integers(V) of
		true  -> are_all_positive_ints2(NewI);
		false -> false
	end.

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
force_index(#'$ast¯'{do   = #'$shape¯'{dimensions = 0}} = AST, _) ->
	AST;
% do work on unindexed arrays
force_index(#'$ast¯'{do   = #'$shape¯'{} = Shp} = AST, Forcing) when Forcing == index  orelse
																																		 Forcing == unindex ->
	AST#'$ast¯'{do = Shp#'$shape¯'{forcing = Forcing}};
% don't do anything to anything else
force_index(X, _) ->
	X.

% don't do anything scalars
make_indexed(#'$ast¯'{do   = #'$shape¯'{dimensions = 0}} = AST) ->
	AST;
% do work on unindexed arrays
make_indexed(#'$ast¯'{do   = #'$shape¯'{indexed = false} = Shp,
											args = Args} = AST) ->
	{_Len, NewArgs} = pometo_runtime:index(Args),
	AST#'$ast¯'{do   = Shp#'$shape¯'{indexed = true},
							args = NewArgs};
% don't do anything to anything else
make_indexed(X) ->
	X.

args_reverse(List) when is_list(List) -> lists:reverse(List);
args_reverse(Map)  when is_map(Map)   ->
	Iter = maps:iterator(Map),
	args_rev2(Iter, ?EMPTY_ACCUMULATOR).

args_rev2(none, Acc) -> Acc;
args_rev2(Iter, Acc) -> {_K, V, NextI} = maps:next(Iter),
												args_rev2(NextI, [V | Acc]).

get_no_of_elements_from_dims(0)       -> 1;
get_no_of_elements_from_dims([N])     -> N;
get_no_of_elements_from_dims([H | T]) -> lists:foldl(fun(X, Acc) -> X * Acc end, H, T).

get_no_of_elements_from_args(0)                    -> 0;
get_no_of_elements_from_args([N])                  -> N;
get_no_of_elements_from_args([H | T])              -> lists:foldl(fun(X, Acc) -> X * Acc end, H, T);
get_no_of_elements_from_args(Map) when is_map(Map) -> Iter = maps:iterator(Map),
																											get_no_of_elems2(Iter, 1).
get_no_of_elems2(none, Acc) -> Acc;
get_no_of_elems2(Iter, Acc) -> {_K, V, I} = maps:next(Iter),
															 get_no_of_elems2(I, V * Acc).

make_dimensions(L) when is_list(L) -> L;
make_dimensions(M) when is_map(M)  -> Iter = maps:iterator(M),
																			make_dims2(Iter, ?EMPTY_ACCUMULATOR).

make_dims2(none, Acc) -> lists:reverse(Acc);
make_dims2(Iter, Acc) -> {_K, V, NextI} = maps:next(Iter),
												 make_dims2(NextI, [V | Acc]).

args_to_list(List) when is_list(List) -> List;
args_to_list(Map)  when is_map(Map)   -> List = maps:to_list(Map),
																				 {_Keys, Vals} = lists:unzip(List),
																				 Vals;
args_to_list(X)                       -> [X].

snip_args(List, N) when is_list(List) -> {Keep, _Discard} = lists:split(N, List),
																				 Keep;
snip_args(Map, 0)  when is_map(Map)   -> #{};
snip_args(Map, N)  when is_map(Map)   -> Iter = maps:iterator(Map),
																				 snip_map(Iter, 0, N, #{}).

snip_map(_Iter, N,  N, Acc) -> Acc;
snip_map(Iter,  _K, N, Acc) -> {NewK, V, NewI} = maps:next(Iter),
															 snip_map(NewI, NewK, N, maps:put(NewK, V, Acc)).

extend(List, _Start, N, TopUp, _Rem) when is_list(List) -> lists:flatten(lists:duplicate(N, List)) ++ TopUp;
extend(Map,  Start,  N, TopUp, Rem)  when is_map(Map)   ->
	End  = Start * N,
	Iter = maps:iterator(Map),
	NewMap = extend2(Iter, Map, Start + 1, End, Map),
	% now top up
	Iter2 = maps:iterator(TopUp),
	extend2(Iter2, TopUp, End + 1, End + Rem, NewMap).

extend2(_,    _Map, N, End, Acc) when N > End -> Acc;
extend2(none, Map,  N, End, Acc)            	-> NewIter = maps:iterator(Map),
																								 extend2(NewIter, Map, N, End, Acc);
extend2(Iter, Map,  N, End, Acc) 							-> {_K, V, I} = maps:next(Iter),
																								 NewAcc = maps:put(N, V, Acc),
																								 extend2(I, Map, N + 1, End, NewAcc).

check_rank(D1, D1) 						 when is_list(D1)							 -> identical;
check_rank(unsized_vector, D1) when is_list(D1) 						 -> unsized_vector;
check_rank(D1, unsized_vector) when is_list(D1)              -> unsized_vector;
check_rank(unsized_vector, unsized_vector)                   -> unsized_vector;
check_rank(D1, D2) 						 when is_list(D1) andalso
																		is_list(D2) andalso
																		length(D1) == length(D2) -> invalid_rank;
check_rank(D1, D2) 						 when is_list(D1) andalso
																		is_list(D2) andalso
																		length(D1) > length(D2)  -> {D, _Discard} = lists:split(length(D2), D1),
																																case D of
																																	D2 -> subrank;
																																	_  -> invalid_rank
																																end;
check_rank(D1, D2) 						 when is_list(D1) andalso
																		is_list(D2) andalso
																		length(D1) < length(D2)  -> {D, _Discard} = lists:split(length(D1), D2),
																																case D of
																																	D1 -> superrank;
																																	_  -> invalid_rank
																																end;
check_rank(_, _)            	    													 -> notvectors.

maybe_make_vector(#'$ast¯'{do   = #'$shape¯'{dimensions = 0},
										 args = A} = AST) ->
	AST#'$ast¯'{do   = #'$shape¯'{dimensions = [1]},
							args = [A]};
maybe_make_vector(X) ->
	X.

%% anybody is forced index, everbody is forced index
choose_accumulator(#'$ast¯'{do = #'$shape¯'{forcing = F1}},
									 #'$ast¯'{do = #'$shape¯'{forcing = F2}}) when F1 == index orelse
																																 F2 == index         -> #{};
% if one is forced unindex and the other one isn't then force unindex
choose_accumulator(#'$ast¯'{do = #'$shape¯'{forcing = F1}},
									 #'$ast¯'{do = #'$shape¯'{forcing = F2}}) when F1 == unindex orelse
																																 F2 == unindex       -> [];
choose_accumulator(#'$ast¯'{do = #'$shape¯'{forcing = F1}},
									 #'$ast¯'{do = #'$shape¯'{forcing = F2}}) when F1 == unindex orelse
																																 F2 == unindex       -> [];
% if there are no forcing hints and at least one is indexed lets index
choose_accumulator(#'$ast¯'{do = #'$shape¯'{indexed = I1}},
									 #'$ast¯'{do = #'$shape¯'{indexed = I2}}) when I1 == true orelse
																																 I2 == true					 -> #{};
% otherwise unindex
choose_accumulator(_AST1, _AST2)																		 								 -> [].

set_return_type(#'$ast¯'{do = Shp} = AST, Map)  when is_map(Map)   -> AST#'$ast¯'{do = Shp#'$shape¯'{indexed = true}};
set_return_type(#'$ast¯'{do = Shp} = AST, List) when is_list(List) -> AST#'$ast¯'{do = Shp#'$shape¯'{indexed = false}}.

maybe_reverse(Map)  when is_map(Map)   -> Map;
maybe_reverse(List) when is_list(List) -> lists:reverse(List).

%%
%% Exported for use in compiled modules
%%

dyadic_fn(Args)  -> pometo_runtime_dyadic_fn:dyadic_RUNTIME(Args).

monadic_fn(Args) -> pometo_runtime_monadic_fn:monadic_RUNTIME(Args).

dyadic_op(Args)  -> pometo_runtime_dyadic_op:dyadic_op_RUNTIME(Args).

monadic_op(Args) -> pometo_runtime_monadic_op:monadic_op_RUNTIME(Args).


apply_fn([Mod, Fun, Arg]) -> Mod:Fun(Arg).

runtime_let([#'$ast¯'{do = runtime_let, args = Args}]) -> run_ast2(Args).
