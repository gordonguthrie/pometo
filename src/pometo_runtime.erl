-module(pometo_runtime).

%% exported for inclusion in compiled modules
%% should never be called directly
-export([
          runtime_let/1,
          apply_fn/1,
          dyadic/1,
          monadic/1,
          dyadic_ranked/1,
          monadic_ranked/1,
          ambivalent/1,
          resolve_monadic_fork/1,
          run_maybe_monadic_train/1,
          run_maybe_dyadic_train/1,
          run_right_associative/1,
          make_right_associative/1
        ]).

%% things exported for runtime
-export([
          run_ast/2,
          are_all_positive_integers/1,
          product/1,
          index/1,
          make_eager/1,
          make_indexed/1,
          make_unindexed/1,
          unindex/1,
          force_index/2,
          args_reverse/1,
          get_no_of_elements_from_args/1,
          get_no_of_elements_from_dims/1,
          make_dimensions/1,
          args_to_list/1,
          snip_args/2,
          extend/5,
          maybe_cast_scalar_to_vector/1,
          choose_accumulator/2,
          set_return_type/2,
          maybe_reverse/1,
          get_nth/2,
          resolve_rank/2,
          foldl/3,
          get_first/1,
          is_terminated/1,
          make_enumerable/1,
          make_axes/1,
          make_count/1,
          increment_count/2,
          make_index_from_count/2,
          eliminate_rank/2,
          resize_axes/2,
          offset_count/3,
          axes_to_dims/1,
          delete_dim_from_count/2,
          maybe_collapse_identity_arrays/1
        ]).

% things exported for testing only

-export([
         make_vector_TEST/2
        ]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").
-include("runtime_include.hrl").

%%
%% Exported for use in compiled modules
%%

dyadic([Fn, [Arg1, Arg2]]) -> % io:format("in pometo_runtime calling diadic with~n- ~p~n", [[Fn, [Arg1, Arg2]]]),
                              NewArg1 = run_ast2(Arg1),
                              NewArg2 = run_ast2(Arg2),
                              pometo_runtime_dyadic:dyadic_RUNTIME([Fn, [NewArg1, NewArg2]]).

monadic([Fn, [Arg]]) -> % io:format("in pometo_runtime calling monadic with~n- ~p~n", [[Fn, [Arg]]]),
                        NewArg = run_ast2(Arg),
                        pometo_runtime_monadic:monadic_RUNTIME([Fn, [NewArg]]).

dyadic_ranked([Fn, [Arg1, Arg2]]) -> % io:format("in pometo_runtime calling diadic (ranked) with~n- ~p~n", [[Fn, [Arg1, Arg2]]]),
                                     NewArg1 = run_ast2(Arg1),
                                     NewArg2 = run_ast2(Arg2),
                                     pometo_runtime_dyadic:dyadic_RUNTIME([Fn, [NewArg1, NewArg2]]).

monadic_ranked([Fn, [Arg]]) -> % io:format("in pometo_runtime calling monadic (ranked) with~n- ~p~n", [[Fn, [Arg]]]),
                               NewArg = run_ast2(Arg) ,
                               pometo_runtime_monadic:monadic_RUNTIME([Fn, [NewArg]]).

ambivalent([Fn, [Arg]])        -> % io:format("in pometo_runtime calling ambivalent to monadic with~n- ~p~n- ~p~n", [Fn, Arg]),
                                  NewArg = run_ast2(Arg),
                                  pometo_runtime_monadic:monadic_RUNTIME([Fn, [NewArg]]);
ambivalent([Fn, [Arg1, Arg2]]) -> % io:format("in pometo_runtime calling ambivalent to dyadic with~n- ~p~n- ~p~n- ~p~n", [Fn, Arg1, Arg2]),
                                  NewArg1 = run_ast2(Arg1),
                                  NewArg2 = run_ast2(Arg2),
                                  pometo_runtime_dyadic:dyadic_RUNTIME([Fn, [NewArg1, NewArg2]]).

apply_fn([[{Mod, Fun}], Args]) -> % io:format("Applying {~p : ~p} with args~n- ~p~n", [Mod, Fun, Args]),
                                  NewArgs = [run_ast2(X) || X <- Args],
                                  Mod:Fun(NewArgs).

% This is an Afg monadic fork
resolve_monadic_fork([#'$ast¯'{do   = let_op,
                              args = [_VarW, Val]},
                      #'$ast¯'{} = LHS,
                      Mid,
                      RHS]) ->
  NewRHS = run_ast2(RHS#'$ast¯'{args = [Val]}),
  NewMid = run_ast2(Mid#'$ast¯'{args = [LHS, NewRHS]}),
  run_ast2(NewMid);
% this is an fgh monadic fork
resolve_monadic_fork([#'$ast¯'{do   = let_op,
                              args = [_VarW, Val]},
                      #'$func¯'{line_no = LNo,
                                char_no = CNo} = LHS,
                      Mid,
                      RHS]) ->
  NewLHS = run_ast2(#'$ast¯'{do      = LHS,
                             args    = [Val],
                             line_no = LNo,
                             char_no = CNo}),
  NewRHS = run_ast2(RHS#'$ast¯'{args = [Val]}),
  NewMid = Mid#'$ast¯'{args = [NewLHS, NewRHS]},
  run_ast2(NewMid).

runtime_let([#'$ast¯'{do = runtime_let, args = Args}]) -> run_ast2(Args).

run_maybe_monadic_train([#'$ast¯'{do   = #'$shape¯'{dimensions = [1],
                                                    type = func},
                                  args = [Arg]}                    = LHS,
                         #'$ast¯'{do   = #'$shape¯'{}}            = W]) ->
  #'$ast¯'{do   = #'$func¯'{} = Func,
           args = Args}               = Arg,
  NewArgs = Args ++ [W],
  Type = case length(NewArgs) of
    1 -> monadic;
    2 -> dyadic
  end,
  NewAST = Arg#'$ast¯'{do   = Func#'$func¯'{type = Type},
                       args = NewArgs},
  run_ast2(NewAST);
run_maybe_monadic_train([#'$ast¯'{do   = #'$shape¯'{type = func},
                                  args = Args}                    = LHS,
                         #'$ast¯'{do   = #'$shape¯'{}}            = W]) ->
  NewAST = make_train(lists:reverse(Args), monadic, [W]),
  run_ast2(NewAST);
run_maybe_monadic_train([#'$ast¯'{do   = #'$shape¯'{type = maybe_func} = Shp,
                                  args = Args}                            = LHS,
                         #'$ast¯'{do   = #'$shape¯'{}}                    = W]) ->
  case type_array(Args) of
    func  -> run_maybe_monadic_train([LHS#'$ast¯'{do = Shp#'$shape¯'{type = func}}, W]);
    shape -> make_vector(LHS, W);
    mixed -> Trains = make_trains(lists:reverse(Args), monadic, [W]),
             case Trains of
                #'$ast¯'{do = #'$shape¯'{}} -> Trains;
                _                           -> run_ast2(Trains)
             end
  end;
run_maybe_monadic_train([#'$ast¯'{}        = LHS,
                         #'$ast¯'{do = Do} = W]) when is_record(Do, '$shape¯') orelse
                                                      is_record(Do, '$func¯')  ->
  ExpandedAST = expand_maybe_train(LHS, W),
  case ExpandedAST of
    #'$ast¯'{do = #'$shape¯'{}} -> ExpandedAST;
    #'$ast¯'{do = #'$func¯'{}}  -> run_ast2(ExpandedAST)
  end.

run_maybe_dyadic_train([#'$ast¯'{do   = #'$shape¯'{type = func},
                                 args = Args}                    = _LHS,
                         #'$ast¯'{do  = #'$shape¯'{}}            = A,
                         #'$ast¯'{do  = #'$shape¯'{}}            = W]) ->
  NewAST = make_train(lists:reverse(Args), dyadic, [A, W]),
  run_ast2(NewAST);
run_maybe_dyadic_train([#'$ast¯'{do   = #'$shape¯'{type = maybe_func},
                                  args = Args}                          = LHS,
                         #'$ast¯'{do  = #'$shape¯'{}}                   = A,
                         #'$ast¯'{do   = #'$shape¯'{}}                  = W]) ->
  case type_array(Args) of
    func  -> run_maybe_dyadic_train([LHS#'$ast¯'{do = #'$shape¯'{type = func}}, A, W]);
    shape -> exit(shapeoh2);
    mixed -> Trains = make_trains(lists:reverse(Args), dyadic, [A, W]),
             run_ast2(Trains)
  end.

expand_maybe_train(#'$ast¯'{do = #'$shape¯'{}} = AST,
                   #'$ast¯'{do = #'$shape¯'{}} = W) ->
  merge_shapes(AST, W);
expand_maybe_train(#'$ast¯'{do   = #'$func¯'{},
                            args = []} = AST,
                  #'$ast¯'{do = #'$shape¯'{}} = W) ->
  AST#'$ast¯'{args = [W]};
expand_maybe_train(#'$ast¯'{do   = #'$shape¯'{dimensions = [N],
                                              type = maybe_func} = Shp,
                            args = Args} = AST,
                   #'$ast¯'{do = #'$func¯'{}} = W) ->
  AST#'$ast¯'{do   = Shp#'$shape¯'{dimensions = [N + 1]},
              args = Args ++ [W]}.

merge_shapes(#'$ast¯'{do   = #'$shape¯'{dimensions = 0} = Shp1,
                      args = Arg1} = AST,
             #'$ast¯'{do   = #'$shape¯'{dimensions = 0},
                      args = Arg2}) ->
  AST#'$ast¯'{do   = Shp1#'$shape¯'{dimensions = [2],
                                    indexed    = false,
                                    type       = runtime},
              args = [Arg1, Arg2]};
merge_shapes(#'$ast¯'{do   = #'$shape¯'{dimensions = 0} = Shp1,
                      args = Arg1} = AST,
             #'$ast¯'{do   = #'$shape¯'{dimensions = [_N]},
                      args = Arg2}) ->
  AccType = get_acc_type(Arg2),
  Args = make_list(Arg1, Arg2),
  {NewN, NewArgs, IsIndexed} = maybe_flatten(Args, AccType),
  AST#'$ast¯'{do   = Shp1#'$shape¯'{dimensions = [NewN],
                                    indexed    = IsIndexed,
                                    type       = runtime},
              args = NewArgs};
merge_shapes(#'$ast¯'{do   = #'$shape¯'{dimensions = 0} = Shp1,
                      args = Arg1} = AST1,
             #'$ast¯'{do   = #'$shape¯'{dimensions = unsized_vector},
                      args = Arg2} = AST2) ->
  {NewN, NewArgs, IsIndexed} = maybe_flatten([Arg1 | Arg2], list),
  AST1#'$ast¯'{do   = Shp1#'$shape¯'{dimensions = [NewN],
                                     indexed    = IsIndexed,
                                     type       = runtime},
               args = NewArgs};
merge_shapes(#'$ast¯'{do   = #'$shape¯'{indexed = IsIndexed1} = Shp1,
                      args = Arg1} = AST1,
             #'$ast¯'{do   = #'$shape¯'{indexed = IsIndexed2},
                      args = Arg2} = AST2) ->
  IsIndexed = IsIndexed1 and IsIndexed2,
  AccType = case IsIndexed of
    true  -> map;
    false -> list
  end,
  Args = make_list(Arg1, Arg2),
  {NewN, NewArgs, FinalIsIndexed} = maybe_flatten(Args, AccType),
  AST1#'$ast¯'{do   = Shp1#'$shape¯'{dimensions = [NewN],
                                     indexed    = FinalIsIndexed,
                                     type       = runtime},
               args = NewArgs}.

make_vector_TEST(AST1, AST2) -> make_vector(AST1, AST2).

make_vector([#'$ast¯'{do = #'$shape¯'{}} = AST], _CNo, _LNo) ->
  AST;
make_vector(List, CNo, LNo) when is_list(List) ->
  Dims = [length(List)],
  Ret = #'$ast¯'{do      = #'$shape¯'{dimensions = Dims,
                                      type       = runtime,
                                      char_no    = CNo,
                                      line_no    = LNo},
                 args    = List,
                 char_no = CNo,
                 line_no = LNo},
  Ret.

make_vector(#'$ast¯'{do      = #'$shape¯'{type = maybe_func},
                     args    = Args,
                     char_no = CNo,
                     line_no = LNo},
            #'$ast¯'{do   = #'$shape¯'{}} = RHS) ->
  Acc = #'$ast¯'{do      = #'$shape¯'{dimensions = [0],
                                      type       = runtime,
                                      char_no    = CNo,
                                      line_no    = LNo},
                 args    = [],
                 char_no = CNo,
                 line_no = LNo},
  NewArgs = case is_list(Args) of
    true  -> Args ++ [RHS];
    false -> [Args, RHS]
  end,
  make_vector2(NewArgs, Acc).

make_vector2([], Acc) ->
  Acc;
make_vector2([#'$ast¯'{do   = ?shp(0),
                       args = Arg} | T],
              #'$ast¯'{do   = ?shp([N]),
                       args = Args} = Acc) ->
  NewAcc = Acc#'$ast¯'{do = ?shp([N + 1]),
                       args = Args ++ [Arg]},
  make_vector2(T, NewAcc);
make_vector2([#'$ast¯'{do   = ?shp([_N]) = Shape} = H | #'$ast¯'{do = ?shp([_M])} =T],
              #'$ast¯'{do   = ?shp([N]),
                       args = Args} = Acc)
  when is_record(Shape, '$shape¯') ->
  {Head, [Tail]} = lists:split(length(Args) - 1, Args),
  {NewTail, NewDims} = case Tail of
      #'$ast¯'{} -> #'$ast¯'{do   = ?shp([M]) = Shp,
                             args = InnerArgs} = Tail,
                    {[Tail#'$ast¯'{do   = Shp?shp([M + 1]),
                                   args = [H | InnerArgs]}], N};
      _          -> {[Tail, H], N + 1}
  end,
  NewAcc = Acc#'$ast¯'{do   = ?shp([NewDims]),
                       args = Head ++ NewTail},
  make_vector2(T, NewAcc);
% otherwise just accumulate at the top level
make_vector2([#'$ast¯'{} = H | T],
              #'$ast¯'{do   = ?shp([N]),
                       args = Args} = Acc) ->
  NewAcc = Acc#'$ast¯'{do = ?shp([N + 1]),
                       args = Args ++ [H]},
  make_vector2(T, NewAcc).


make_list(List1, List2) when is_list(List1) andalso
                             is_list(List2) -> List1 ++ List2;
make_list(List1, Map2)  when is_list(List1) andalso
                             is_map(Map2)   -> List1 ++ [Map2];
make_list(Map1,  List2) when is_map(Map1)   andalso
                             is_list(List2) -> [Map1 | List2];
make_list(X,     List2) when is_list(List2) -> [X | List2];
make_list(X1,    X2)                        -> [X1, X2].

get_acc_type(List) when is_list(List) -> list;
get_acc_type(Map)  when is_map(Map)   -> map;
get_acc_type(_X)                      -> list.

maybe_flatten(Args, Type) ->
  Acc = case Type of
    list -> ?EMPTY_ACCUMULATOR;
    map  -> ?EMPTY_MAP
  end,
  {NewN, Flattened} = case can_flatten(Args) of
    true  -> maybe_f2(Args, 1, Acc);
    false -> consolidate(maybe_unindex(Args), 1, ?shp([0]), ?EMPTY_ACCUMULATOR)
  end,
  % true means the result is indexed, false it isn't...
  if
    is_map(Flattened)  -> {NewN, Flattened, true};
    is_list(Flattened) -> {NewN, Flattened, false}
  end.

maybe_f2([], N, Args) when is_list(Args) -> {N - 1, lists:reverse(Args)};
maybe_f2([], N, Args) when is_map(Args)  -> {N - 1, Args};
maybe_f2([#'$ast¯'{do   = #'$shape¯'{dimensions = 0},
                   args = Arg} | T], N, Acc) ->
  maybe_f2(T, N + 1, accumulate(N, Arg, Acc));
maybe_f2([Map | T], N, Acc) when is_map(Map) ->
  Iter = maps:iterator(Map),
  {NewN, NewAcc} = iterate_over_map(Iter, N, Acc),
  maybe_f2(T, NewN, NewAcc);
maybe_f2([H | T], N, Acc) ->
  maybe_f2(T, N + 1, accumulate(N, H, Acc)).

maybe_unindex(Args) -> maybe_unindex2(Args, ?EMPTY_ACCUMULATOR).

maybe_unindex2([],        Acc)                  -> lists:flatten(lists:reverse(Acc));
maybe_unindex2([Map | T], Acc) when is_map(Map) -> maybe_unindex2(T, [unindex(Map) | Acc]);
maybe_unindex2([X   | T], Acc)                  -> maybe_unindex2(T, [X            | Acc]).

iterate_over_map(none, N, Acc) -> {N, Acc};
iterate_over_map(Iter, N, Acc) ->
  {_K, V, NewIter} = maps:next(Iter),
  Val = case V of
      #'$ast¯'{do   = #'$shape¯'{dimensions = 0},
               args = Arg} -> Arg;
      _                    -> V
  end,
  NewAcc = accumulate(N, Val, Acc),
  iterate_over_map(NewIter, N + 1, NewAcc).

consolidate([], N, ?shp([0]), Acc) ->
  {N - 1, lists:reverse(Acc)};
% if there is no accumulator we want to emerge the list into a single vector
consolidate([], N, #'$ast¯'{args = Args} = Partial, []) ->
  % we accumulate the args in the partial in reverse order
  % so reverse 'em
  % also get a real length
  {length(Args), lists:reverse(Args)};
consolidate([], N, #'$ast¯'{args = Args} = Partial, Acc) ->
  % we accumulate the args in the partial in reverse order
  % so reverse 'em
  NewP = Partial#'$ast¯'{args = lists:reverse(Args)},
  {N - 1, lists:reverse([NewP | Acc])};
consolidate([#'$ast¯'{do = ?shp(Dim)} = H | T], N, none, Acc) when Dim /= 0 ->
  consolidate(T, N + 1, none, [H | Acc]);
consolidate([H | T], N, none, Acc) ->
  NewArg = maybe_extract_from_scalar(H),
  Partial = #'$ast¯'{do   = ?shp([1]),
                     args = [NewArg]},
  consolidate(T, N + 1, Partial, Acc);
consolidate([#'$ast¯'{do = ?shp(Dim)} = H | T], N, #'$ast¯'{args = Args} = Partial, Acc) when Dim /= 0 ->
  NewP = Partial#'$ast¯'{args = lists:reverse(Args)},
  consolidate(T, N + 1, none, [H, NewP | Acc]);
consolidate([H | T], N, #'$ast¯'{do   = ?shp([Dim]),
                                 args = Args}, Acc) ->
  % we don't bump N when we consolidate in a partial
  NewArg = maybe_extract_from_scalar(H),
  NewP = #'$ast¯'{do   = ?shp([Dim + 1]),
                  args = [NewArg | Args]},
  consolidate(T, N, NewP, Acc).

maybe_extract_from_scalar(#'$ast¯'{do = ?shp(0),
                                   args = Arg}) -> Arg;
maybe_extract_from_scalar(#'$ast¯'{} = AST)     -> AST;
maybe_extract_from_scalar(X)                    -> X.

can_flatten([])                           -> true;
can_flatten([#'$ast¯'{do = ?scalar} | T]) -> can_flatten(T);
can_flatten([Map | T])  when is_map(Map)  -> Iter = maps:iterator(Map),
                                             case can_flatten2(Iter) of
                                                true  -> can_flatten(T);
                                                false -> false
                                             end;
can_flatten(_X)                           -> false.

can_flatten2(none) -> true;
can_flatten2(Iter) -> {_K, V, NewI} = maps:next(Iter),
                      case V of
                        ?scalar    -> can_flatten2(NewI);
                        #'$ast¯'{} -> false;
                        _          -> can_flatten2(NewI)
                      end.

accumulate(_N, Val, List) when is_list(List) -> [Val | List];
accumulate(N,  Val, Map)  when is_map(Map)   -> maps:put(N, Val, Map).

make_trains(List, Type, Operands) ->
  {NewList, NewOperands} = accumulate_right(Operands ++ List, ?EMPTY_ACCUMULATOR),
  make_trains2(NewList, Type, NewOperands, ?EMPTY_ACCUMULATOR, ?EMPTY_ACCUMULATOR).

accumulate_right([#'$ast¯'{do = #'$shape¯'{}} = H | T], Acc) ->
  accumulate_right(T, [H | Acc]);
accumulate_right(Rest, Acc) ->
  #'$ast¯'{char_no = CNo,
           line_no = LNo} = hd(Acc),
  {Rest, make_vector(Acc, CNo, LNo)}.

make_trains2([], Type, _Operands, [], Acc) ->
  case length(Acc) of
    1 -> [AST] = Acc,
         AST;
    _ ->   case type_array(Acc) of
              func  -> make_trains2(Acc, Type, [], ?EMPTY_ACCUMULATOR, ?EMPTY_ACCUMULATOR);
              shape -> #'$ast¯'{char_no = CNo,
                                line_no = LNo} = hd(Acc),
                       make_vector(Acc, CNo, LNo)
           end
  end;
make_trains2([#'$ast¯'{do = #'$func¯'{}} = H | T], Type, Operands, Partial, Acc) ->
  make_trains2(T, Type, Operands, [H | Partial], Acc);
make_trains2([H | T], Type, Operands, Partial, Acc) ->
  NewAcc = process_partial(H, Operands, Type, Partial),
  make_trains2(T, Type, Operands, ?EMPTY_ACCUMULATOR, [NewAcc | Acc]).

process_partial(H, _Operands, _Type, []) ->
  H;
process_partial(H, Operands,  _Type, [#'$ast¯'{do   = #'$func¯'{} = Func,
                                               args = []} = AST]) ->
  AST#'$ast¯'{do   = Func#'$func¯'{type = dyadic},
              args = [H, Operands]};
process_partial(H, Operands, Type, Partial) ->
        make_Agh_fork(H, Partial, Type, Operands).

make_Agh_fork(#'$ast¯'{do = #'$shape¯'{}} = AST, Funs, Type, Operands) ->
  Len = length(Funs),
  case is_even(Len) of
    true ->
      Train = make_train(lists:reverse(Funs), Type, Operands),
      #'$ast¯'{do   = #'$func¯'{} = Func,
               args = Args} = Train,
      Ret = Train#'$ast¯'{do = Func#'$func¯'{type = dyadic},
                          args = [AST | Args]},
      Ret;
    false ->
      #'$ast¯'{char_no = CNo,
               line_no = LNo} = AST,
      Error  = "SYNTAX ERROR",
      Msg1   = "Missing right argument",
      Msg2   = "Cannot have a scalar or array as the LHS of a train with an odd number of functions (ie an atop)",
      Err    = pometo_runtime_errors:make_error(Error, Msg1, Msg2, LNo, CNo),
      throw({error, Err})
  end.

-record(types, {has_functions = false,
                has_shapes    = false}).

type_array(ASTs) when is_list(ASTs) ->
  case type_array2(ASTs, #types{}) of
    #types{has_functions = true,
           has_shapes    = true}  -> mixed;
    #types{has_functions = true,
           has_shapes    = false} -> func;
    #types{has_functions = false,
           has_shapes    = true}  -> shape
  end.

type_array2([], Acc)                                   -> Acc;
type_array2([#'$ast¯'{do = #'$func¯'{}}  | Rest], Acc) -> type_array2(Rest, Acc#types{has_functions = true});
type_array2([#'$ast¯'{do = #'$shape¯'{}} | Rest], Acc) -> type_array2(Rest, Acc#types{has_shapes = true}).

make_train([Final], _Type,  _Operands) ->
  Final;
% fork
make_train([RHS, Mid, LHS | Rest], Type, Operands) ->
  NewLHS = pometo_parser:descend_arg(LHS, Type, Operands),
  NewRHS = pometo_parser:descend_arg(RHS, Type, Operands),
  #'$ast¯'{do = Func} = Mid,
  NewAcc = Mid#'$ast¯'{do   = Func#'$func¯'{type = dyadic},
                       args = [NewLHS, NewRHS]},
  make_train([NewAcc | Rest], Type, Operands);
% atop
make_train([RHS, LHS], Type, Operands) ->
#'$ast¯'{do = Func} = LHS,
NewRHS = pometo_parser:descend_arg(RHS, Type, Operands),
LHS#'$ast¯'{do   = Func#'$func¯'{type = Type},
            args = [NewRHS]}.

run_right_associative([#'$ast¯'{do   = #'$shape¯'{type = Type} = Shp,
                                args = Args}                    = Funcs])
  when Type == maybe_func ->
  case lists:last(Args) of
    #'$ast¯'{do = #'$shape¯'{}} ->
      NewAST = make_right_associative(Funcs#'$ast¯'{do = Shp#'$shape¯'{type = func}}),
      run_ast2(NewAST);
    X ->
      #'$ast¯'{line_no = LNo,
               char_no = CNo} = lists:last(Args),
      ErrType = "SYNTAX ERROR",
      Msg1    = "invalid expression",
      Msg2    = "no value at the RHS",
      Error   = pometo_runtime_errors:make_error(ErrType, Msg1, Msg2, LNo, CNo),
      throw({error, Error})
  end;
run_right_associative([#'$ast¯'{do   = #'$shape¯'{type = Type} = Shp} = Funcs,
                       #'$ast¯'{do   = #'$shape¯'{}}                  = RHS])
  when Type == func       orelse
       Type == maybe_func ->
  NewFn = make_right_associative(Funcs#'$ast¯'{do = Shp#'$shape¯'{type = func}}),
  #'$ast¯'{do   = #'$func¯'{} = F,
           args = NewArgs} = NewFn,
  {NewType, NewArgs2} = case NewArgs of
    [] -> {monadic, [RHS]};
    _  -> {dyadic,  NewArgs ++ [RHS]}
  end,
  NewAST = NewFn#'$ast¯'{do   = F#'$func¯'{type = NewType},
                         args = NewArgs2},
  run_ast2(NewAST);
run_right_associative([#'$ast¯'{do = #'$func¯'{}} = AST]) ->
  run_ast2(AST);
run_right_associative([#'$ast¯'{do = #'$shape¯'{}} = AST]) ->
  AST.

make_right_associative(#'$ast¯'{do   = #'$shape¯'{type = Type},
                                args = Funcs}) when Type == func       orelse
                                                    Type == maybe_func ->
  make_right_assoc2(lists:reverse(Funcs)).

make_right_assoc2([Final]) ->
  Final;
make_right_assoc2([#'$ast¯'{do = #'$func¯'{}}        = RHS,
                   #'$ast¯'{do = #'$func¯'{} = Func} = LHS | Rest]) ->
  NewHead = LHS#'$ast¯'{do   = Func#'$func¯'{type = monadic},
                        args = [RHS]},
  make_right_assoc2([NewHead | Rest]);
make_right_assoc2([#'$ast¯'{do   = #'$shape¯'{}} = RHS,
                   #'$ast¯'{do   = #'$func¯'{},
                            args = Args}         = LHS | Rest]) ->
  #'$ast¯'{do = #'$func¯'{} = Func} = LHS,
  {NewType, NewArgs} = case Args of
    [] -> {monadic, [RHS]};
    _  -> {dyadic,  Args ++ [RHS]}
  end,
  NewHead = LHS#'$ast¯'{do   = Func#'$func¯'{type = NewType},
                        args = NewArgs},
  make_right_assoc2([NewHead | Rest]);
make_right_assoc2([#'$ast¯'{do   = #'$func¯'{},
                            args = Args}         = RHS,
                   #'$ast¯'{do   = #'$shape¯'{}} = LHS | Rest]) ->
  #'$ast¯'{do = #'$func¯'{} = Func} = RHS,
  {NewType, NewArgs} = case Args of
    [] -> {monadic, [LHS]};
    _  -> {dyadic,  [LHS | Args]}
  end,
  NewHead = RHS#'$ast¯'{do   = Func#'$func¯'{type = NewType},
                        args = NewArgs},
  make_right_assoc2([NewHead | Rest]).

%%
%% Runtime API
%%

run_ast(AST, Str) ->
  % io:format("in run_ast AST is ~p~n", [AST]),
  try run_ast2(AST)
  catch
    throw:E -> {error, #error{} = Err} = E,
               {error, Err#error{expr = Str}}
  end.

run_ast2(#'$ast¯'{do   = [{apply_fn, {Mod, Fn}}],
                  args = Args}) when is_list(Args)->
  % io:format("in run_ast2 (1) Apply (~p:~p)~n- to Args is ~p~n", [Mod, Fn, Args]),
  apply_fn([[{Mod, Fn}], Args]);
run_ast2(#'$ast¯'{do = #'$shape¯'{type = Type}} = AST) when Type /= maybe_func andalso
                                                            Type /= func       ->
  % io:format("in run_ast2 (2) AST is ~p~n", [AST]),
  AST;
run_ast2(#'$ast¯'{do      = #'$shape¯'{type = Type} = Shp,
                  args    = Args,
                  char_no = CNo,
                  line_no = LNo} = Funcs) when Type == maybe_func ->
  NewArgs = [run_ast2(X) || X <- Args],
  case type_array(NewArgs) of
    func  -> exit(rapidelino);
    shape -> make_vector(NewArgs, CNo, LNo);
    mixed -> {MaybeTrain, [RHS]} = lists:split(length(NewArgs) - 1, Args),
             Maybe2 = Funcs#'$ast¯'{do   = Shp#'$shape¯'{dimensions = [length(MaybeTrain)],
                                                         type       = maybe_func},
                                    args = MaybeTrain},
             run_maybe_monadic_train([Maybe2, RHS])
  end;
run_ast2(#'$ast¯'{do   = 'let_op',
                  args = [_V, A | []]} = AST) ->
  NewL = AST#'$ast¯'{do   = runtime_let,
                     args = A},
  % io:format("in run_ast2 (3) AST is ~p~n", [AST]),
  runtime_let([NewL]);
run_ast2(#'$ast¯'{do   = #'$func¯'{type = Type} = Func,
                  args = [A1, A2]})                     when Type == dyadic        orelse
                                                             Type == dyadic_ranked orelse
                                                             Type == ambivalent    ->
  % io:format("in run_ast2 (4)~n", []),
  dyadic([Func, [run_ast2(A1), run_ast2(A2)]]);
run_ast2(#'$ast¯'{do   = #'$func¯'{type = Type} = Func,
                  args = [A]})                          when Type == monadic        orelse
                                                             Type == monadic_ranked orelse
                                                             Type == ambivalent     ->
  % io:format("in run_ast2 (5) Func is ~p ~n- A is ~p~n", [Func, A]),
  monadic([Func, [run_ast2(A)]]);
run_ast2(#'$ast¯'{do   = defer_evaluation,
                  args = [#'$ast¯'{do   = #'$shape¯'{type = variable},
                                   args = #'$var¯'{} = Var}]}) ->
  % io:format("in run_ast2 (6) Var is ~p~n", [Var]),
  Var;
run_ast2(#'$ast¯'{do   = defer_evaluation,
                  args = [#'$ast¯'{do = #'$func¯'{}}]} = AST)  ->
  % io:format("in run_ast2 (7) AST is ~p~n", [AST]),
  AST;
run_ast2(#'$ast¯'{do   = defer_evaluation,
                  args = [#'$ast¯'{do   = #'$shape¯'{},
                                   args = Args}]}) ->
  % io:format("in run_ast2 (8) Args is ~p~n", [Args]),
  Args;
run_ast2(#'$ast¯'{do   = resolve_monadic_fork,
                  args = Args}) ->
  % io:format("in run_ast2 (9) Args is ~p~n", [Args]),
  resolve_monadic_fork(Args);
run_ast2(A) ->
  % io:format("in run_ast2 (10) A is ~p~n", [A]),
  A.

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
    Val = case X of
      #'$ast¯'{do   = #'$shape¯'{dimensions = 0},
               args = Arg} -> Arg;
      _                    -> X
    end,
    NewMap = maps:put(N, Val, Map),
    {N + 1, NewMap}
  end,
  lists:foldl(IndexFn, {1, #{}}, Args).

make_eager(#'$ast¯'{do    = #'$shape¯'{dimensions = unsized_vector} = Shp,
                    args = Args} = AST) ->
  AST#'$ast¯'{do = Shp#'$shape¯'{dimensions = [length(Args)]}};
make_eager(X) ->
  X.

unindex(Args) when is_map(Args) -> List = lists:sort(maps:to_list(Args)),
                   {_Keys, Vals} = lists:unzip(List),
                   Vals.

% don't do anything scalars
force_index(#'$ast¯'{do = ?scalar} = AST, _) ->
  AST;
% do work on unindexed arrays
force_index(#'$ast¯'{do   = #'$shape¯'{} = Shp} = AST, Forcing) when Forcing == index  orelse
                                                                     Forcing == unindex ->
  AST#'$ast¯'{do = Shp#'$shape¯'{forcing = Forcing}};
% don't do anything to anything else
force_index(X, _) ->
  X.

% don't do anything scalars
make_indexed(#'$ast¯'{do = ?scalar} = AST) ->
  AST;
% do work on unindexed arrays that are not func or maybe_func types
make_indexed(#'$ast¯'{do   = #'$shape¯'{indexed = false,
                                        type    = Type} = Shp,
                      args = Args} = AST) when Type /= func       andalso
                                               Type /= maybe_func ->
  {_Len, NewArgs} = pometo_runtime:index(Args),
  AST#'$ast¯'{do   = Shp#'$shape¯'{indexed = true},
              args = NewArgs};
% don't do anything to anything else
make_indexed(X) ->
  X.

% don't do anything scalars
make_unindexed(#'$ast¯'{do = ?scalar} = AST) ->
  AST;
% do work on unindexed arrays that are not func or maybe_func types
make_unindexed(#'$ast¯'{do   = #'$shape¯'{indexed = true} = Shp,
                      args = Args} = AST) ->
  NewArgs = unindex(Args),
  AST#'$ast¯'{do   = Shp#'$shape¯'{indexed = false},
              args = NewArgs};
% don't do anything to anything else
make_unindexed(X) ->
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
get_no_of_elements_from_dims([H | T]) -> lists:foldl(fun(X, Acc) -> X * Acc end, H, T);
get_no_of_elements_from_dims([])      -> 1. % if you reduce dims by rank and get the last it is an empty vector

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
extend2(none, Map,  N, End, Acc)              -> NewIter = maps:iterator(Map),
                                                 extend2(NewIter, Map, N, End, Acc);
extend2(Iter, Map,  N, End, Acc)              -> {_K, V, I} = maps:next(Iter),
                                                 NewAcc = maps:put(N, V, Acc),
                                                 extend2(I, Map, N + 1, End, NewAcc).

maybe_cast_scalar_to_vector(#'$ast¯'{do   = #'$shape¯'{dimensions = 0},
                                     args = A} = AST) ->
  AST#'$ast¯'{do   = #'$shape¯'{dimensions = [1]},
              args = [A]};
maybe_cast_scalar_to_vector(X) ->
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
                                                                 I2 == true          -> #{};
% otherwise unindex
choose_accumulator(_AST1, _AST2)                                                     -> [].

set_return_type(#'$ast¯'{do = Shp} = AST, Map)  when is_map(Map)   -> AST#'$ast¯'{do = Shp#'$shape¯'{indexed = true}};
set_return_type(#'$ast¯'{do = Shp} = AST, List) when is_list(List) -> AST#'$ast¯'{do = Shp#'$shape¯'{indexed = false}}.

maybe_reverse(Map)  when is_map(Map)   -> Map;
maybe_reverse(List) when is_list(List) -> lists:reverse(List).

get_nth(#'$ast¯'{args = L},  N) when is_list(L) -> lists:nth(N, L);
get_nth(#'$ast¯'{args = M},  N) when is_map(M)  -> maps:get(N, M).

resolve_rank(NewD2, Rank) ->
  case Rank of
    none  -> get_length(NewD2);
    first -> get_length(NewD2);
    last  -> 1;
    _     -> Rank
  end.

get_length(unsized_vector)    -> 1;
get_length(L) when is_list(L) -> length(L).

foldl(ApplyFn, [H | Rest] = List, _Len) when is_list(List) -> lists:foldl(ApplyFn, H, Rest);
foldl(ApplyFn, Map,                Len) when is_map(Map)   -> Acc = maps:get(1, Map),
                                                              mapfold(ApplyFn, Map, 2, Len, Acc).

mapfold(_ApplyFn, _Map, Len, Len, Acc) -> Acc;
mapfold(ApplyFn,  Map,  N,   Len, Acc) -> Val = maps:get(N, Map),
                                          NewAcc = ApplyFn(Val, Acc),
                                          mapfold(ApplyFn, Map, N + 1, Len, NewAcc).

is_terminated({map, none}) -> true;
is_terminated({list, []})  -> true;
is_terminated(_)           -> false.

make_enumerable(Map)  when is_map(Map)   -> {map,  maps:iterator(Map)};
make_enumerable(List) when is_list(List) -> {list, List};
make_enumerable(Val)                     -> {list, [Val]}.


get_first({map, Iter})      -> {_K, Val, NewIter} = maps:next(Iter),
                               {{map,  NewIter}, Val};
get_first({list, [H | T]})  -> {{list, T},       H}.

make_axes(List) -> Keys = lists:seq(1, length(List)),
                   maps:from_list(lists:zip(Keys, List)).

make_count(N) -> Keys = lists:seq(1, N),
                 Vals = lists:duplicate(N, 1),
                 {N, maps:from_list(lists:zip(Keys, Vals))}.

increment_count({N, Count}, Axes) ->
  increment_count2({N, N, Count}, Axes).

increment_count2({N, Orig, Count}, Axes) ->
  Val = maps:get(N, Count),
  Max = maps:get(N, Axes),
  if
    Val == Max ->
      if
        N == 1 ->
        '$eof';
      el/=se ->
        NewCount = maps:put(N, 1, Count),
        increment_count2({N - 1, Orig, NewCount}, Axes)
      end;
    el/=se ->
      NewCount = maps:put(N, Val + 1, Count),
      {Orig, NewCount}
  end.

make_index_from_count({N, Count}, Axes) ->
  StartingIndex = 0,
  make_index_from_count2(N, Count, 1, Axes, StartingIndex).

make_index_from_count2(0, _Count, _CurrentSize, _Axes, Index) ->
  Index + 1;
make_index_from_count2(N, Count, CurrentSize, Axes, Index) ->
  Current  = maps:get(N, Count),
  AxisSize = maps:get(N, Axes),
  if
    Current > AxisSize ->
      out_of_bounds;
    Current =< 0 ->
      out_of_bounds;
    el/=se ->
      {NewSize, Incr} = case AxisSize of
          0 -> {CurrentSize,            0};
          _ -> {AxisSize * CurrentSize, (Current - 1) * CurrentSize}
      end,
      make_index_from_count2(N - 1, Count, NewSize, Axes, Incr + Index)
  end.

eliminate_rank(Rank, D) ->
  {Left, [_ | Right]} = lists:split(Rank - 1, D),
  Left ++ Right.

resize_axes([], Axes) ->
  Axes;
resize_axes([{delete, Rank} | T], Axes) ->
  Dims = axes_to_dims(Axes),
  NewDims = eliminate_rank(Rank, Dims),
  NewAxes = make_axes(NewDims),
  resize_axes(T, NewAxes);
resize_axes([{Adjustment, Rank} | T], Axes) ->
  OldVal  = maps:get(Rank, Axes),
  NewVal  = OldVal + Adjustment,
  NewAxes = maps:put(Rank, NewVal, Axes),
  resize_axes(T, NewAxes).

offset_count(Offset, Rank, {N, Count}) ->
  OldVal = maps:get(Rank, Count),
  NewVal = OldVal + Offset,
  if
    NewVal =< 0 -> out_of_bounds;
    el/=se      -> NewCount = maps:put(Rank, NewVal, Count),
                   {N, NewCount}
  end.

axes_to_dims(Map) when is_map(Map) -> {_Keys, Vals} = lists:unzip(lists:sort(maps:to_list(Map))),
                                      Vals.

delete_dim_from_count(Rank, {N, Count}) ->
  % conceptually Counts are <the same> as axes so we can reuse existing fns
  Elements    = axes_to_dims(Count),
  NewElements = eliminate_rank(Rank, Elements),
  NewCount    = make_axes(NewElements),
  {N - 1, NewCount}.

maybe_collapse_identity_arrays(#'$ast¯'{do = ?scalar} = AST) ->
  AST;
maybe_collapse_identity_arrays(#'$ast¯'{do = ?unsized_vector} = AST) ->
  AST;
maybe_collapse_identity_arrays(#'$ast¯'{do = #'$shape¯'{dimensions = D} = Shp} = AST) ->
  NoElems = get_no_of_elements_from_dims(D),
  case NoElems of
    1 -> AST#'$ast¯'{do = Shp#'$shape¯'{dimensions = [1]}};
    _ -> AST
  end;
maybe_collapse_identity_arrays(X) ->
  X.

is_even(N) when is_integer(N) andalso
                N >= 0        -> N band 1 == 0.
