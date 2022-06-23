-module(pometo_runtime_monadic).

%% exported for inclusion in compiled modules
%% should never be called directly
-export([
          monadic_RUNTIME/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").
-include("runtime_include.hrl").

% we need to collapse arrays with dimensions like [1, 1, 1, 1] to [1]
monadic_RUNTIME([Func, [RHS]]) ->
  NewRHS = pometo_runtime:maybe_collapse_identity_arrays(RHS),
  monadic_RUNTIM2([Func, [NewRHS]]).

monadic_RUNTIM2([#'$func¯'{do = [","]},
                 [#'$ast¯'{do   = ?shp(0) = Shp,
                           args = Arg} = AST]]) ->
  NewShp = Shp?shp([1]),
  AST#'$ast¯'{do   = NewShp,
              args = [Arg]};
monadic_RUNTIM2([#'$func¯'{do      = [","],
                           rank    = Rank,
                           line_no = LNo,
                           char_no = CNo},
                 [#'$ast¯'{do      = ?shp(N) = Shp,
                           args    = Args} = AST]]) ->
  try
    NewShp = rerank_ravel(N, Rank),
    AST#'$ast¯'{do   = Shp?shp(NewShp),
                args = Args}
  catch throw:Errs ->
        Msg1 = Errs,
        Msg2 = io_lib:format("~p", [Rank]),
        Error = pometo_errors:make_error("RANK ERROR", Msg1, Msg2, LNo, CNo),
        throw({error, Error})
   end;
monadic_RUNTIM2([#'$func¯'{do = ["⍴"]},
                 [#'$ast¯'{do = ?shp(0) = Shp} = AST]]) ->
  AST#'$ast¯'{do   = Shp,
              args = ""};
% need to know the size for ⍴ so size it and flip it back in
monadic_RUNTIM2([#'$func¯'{do = ["⍴"]} = Func,
                 [#'$ast¯'{do   = ?shp(unsized_vector) = Shp,
                           args = Args} = AST]]) ->
  Dims = [length(Args)],
  monadic_RUNTIM2([Func, [AST#'$ast¯'{do = Shp?shp(Dims)}]]);
monadic_RUNTIM2([#'$func¯'{do = ["⍴"]},
                 [#'$ast¯'{do = ?shp(Dims) = Shp} = AST]]) ->
  NewDims = length(Dims),
  NewShp = Shp#'$shape¯'{dimensions = [NewDims],
                         indexed    = false,
                         type       = number},
  AST#'$ast¯'{do   = NewShp,
              args = Dims};
monadic_RUNTIM2([#'$func¯'{do = ["⍳"]},
                 [#'$ast¯'{do      = ?shp(D),
                           args    = Args,
                           line_no = LNo,
                           char_no = CNo}]]) ->
  NewArgs = case D of
          0 -> [Args];
          _ -> Args
         end,
  case pometo_runtime:are_all_positive_integers(NewArgs) of
    true ->
      NewDs = pometo_runtime:make_dimensions(NewArgs),
      Shp = #'$shape¯'{dimensions = NewDs,
                       indexed    = false,
                       type       = array,
                       line_no    = LNo,
                       char_no    = CNo},
      IsScalar = case D of
        unsized_vector -> false;
        0              -> true;
        _              -> false
      end,
      NewArgs2 = make_args(pometo_runtime:args_reverse(NewArgs), IsScalar, LNo, CNo),
      #'$ast¯'{do      = Shp,
               args    = NewArgs2,
               line_no = LNo,
               char_no = CNo};
    false ->
      Msg1 = "⍳ only accepts integer arguments and was called with",
      Msg2 = io_lib:format("~p", [pometo_runtime:args_to_list(Args)]),
      Error = pometo_errors:make_error("DOMAIN ERROR", Msg1, Msg2, LNo, CNo),
      throw({error, Error})
  end;
monadic_RUNTIM2([#'$func¯'{do = [#'$op¯'{op = Op}]} = Func,
                 [#'$ast¯'{do   = #'$shape¯'{dimensions = 0}} = AST]]) when Op == "/" orelse
                                                                            Op == "⌿" ->
  NewAST = pometo_runtime:maybe_cast_scalar_to_vector(AST),
  monadic_RUNTIM2([Func, [NewAST]]);
monadic_RUNTIM2([#'$func¯'{do   = [#'$op¯'{op = Op, fns = Fns}],
                           rank = Rank} = Func,
                 [#'$ast¯'{do   = #'$shape¯'{} = Shp} = Right]]) when Op == "/" orelse
                                                                      Op == "⌿" ->
  NewRight   = pometo_runtime:make_eager(pometo_runtime:maybe_cast_scalar_to_vector(Right)),
  #'$ast¯'{do = #'$shape¯'{dimensions = NewD}} = NewRight,
  ActualRank = pometo_runtime:resolve_rank(NewD, Rank),
  Axes       = pometo_runtime:make_axes(NewD),
  NewAxes    = pometo_runtime:resize_axes([{delete, ActualRank}], Axes),
  NewFunc    = Func#'$func¯'{do = Fns},
  % we don't use axis in monadic reduce but other ops/fns might
  IterationFn = fun(Val, Count, _Axes, Acc) ->
    NewCount = pometo_runtime:delete_dim_from_count(ActualRank, Count),
    Index    = pometo_runtime:make_index_from_count(NewCount, NewAxes),
    case maps:is_key(Index, Acc) of
      true ->  OldVal = maps:get(Index, Acc),
               NewVal = pometo_runtime_dyadic:execute_dyadic(NewFunc, OldVal, Val),
               maps:put(Index, NewVal, Acc);
      false -> maps:put(Index, Val, Acc)
    end
  end,
  RankFns = #rank_fns{iteration_fn = IterationFn},
  NewArgs = pometo_runtime_rank:iterate_by_axis(NewRight, RankFns),
  NewDims = pometo_runtime:eliminate_rank(ActualRank, NewD),
  case NewDims of
    [] -> NewArg = maps:get(1, NewArgs),
          Right#'$ast¯'{do   = Shp#'$shape¯'{dimensions = 0,
                                             indexed    = false},
                        args = NewArg};
    _ -> Right#'$ast¯'{do    = Shp#'$shape¯'{dimensions = NewDims,
                                             indexed    = true},
                       args = NewArgs}
  end;
monadic_RUNTIM2([#'$func¯'{do = Do},
                 [#'$ast¯'{do   = ?shp(0),
                           args = A} = AST]]) ->
  NewA = do_apply(Do, A, fun execute_monadic/2),
  AST#'$ast¯'{args = NewA};
monadic_RUNTIM2([#'$func¯'{do = Do},
                 [#'$ast¯'{do   = #'$shape¯'{},
                           args = A} = AST]]) ->
  ApplyFun = fun(X) ->
    do_apply(Do, X, fun execute_monadic/2)
  end,
  NewA = apply_to_args(ApplyFun, A),
  AST#'$ast¯'{args = NewA}.

%%
%% Helper functions
%%

do_apply([],      Val, _Fn) -> Val;
do_apply([H | T], Val, Fn)  -> A = Fn(H, Val),
                               do_apply(T, A, Fn).

apply_to_args(Fn, Args) when is_list(Args) -> [Fn(X) || X <- Args];
apply_to_args(Fn, Args) when is_map(Args)  -> I = maps:iterator(Args),
                                              apply_to_map_Val(Fn, Args, I).

apply_to_map_Val(_Fn, Map, none) when is_map(Map) -> Map;
apply_to_map_Val(Fn,  Map, I)    when is_map(Map) -> {K, V, NewI} = maps:next(I),
                                                     NewMap = maps:put(K, Fn(V), Map),
                                                     apply_to_map_Val(Fn, NewMap, NewI).

make_args(Args, IsScalar, LNo, CNo) ->
  NewArgs = make_args2(lists:reverse(Args)),
  %% the indices can either be scalars or vectors depending
  %% on how many arguments are passed to ⍴
  case IsScalar of
    true  -> [X || [X] <- NewArgs];
    false -> Shp = #'$shape¯'{dimensions = 0,
                              type       = array,
                              line_no    = LNo,
                              char_no    = CNo},
             AST = #'$ast¯'{do      = Shp,
                            line_no = LNo,
                            char_no = CNo},
             [AST#'$ast¯'{args = lists:reverse(X)} || X <- NewArgs]
  end.

make_args2([]) -> [];
%make_args2(M) when is_map(M) -> Iterator = maps:iterator(M),
%                               make_args3(Iterator);
make_args2([H | T])          -> Seq = lists:seq(1, H),
                                SecondSeq = make_args2(T),
                                case SecondSeq of
                                  [] -> [[X]     || X <- Seq];
                                  _  -> [[X | Y] || X <- Seq,
                                                    Y <- SecondSeq]
                                end.

%% complex nos first
execute_monadic("+", #'$ast¯'{do   = complex,
                              args = [R, I]} = A) -> A#'$ast¯'{args = [ R, -I]};
execute_monadic("-", #'$ast¯'{do   = complex,
                              args = [R, I]} = A) -> A#'$ast¯'{args = [-R, -I]};
execute_monadic("×", #'$ast¯'{do   = complex,
                              args = [R, I]} = A) -> Mag = math:sqrt(R * R + I * I),
                              A#'$ast¯'{args = unit_tensor(R,I,Mag)};
execute_monadic("÷", #'$ast¯'{do   = complex,
                              args = [R, I]} = A) -> Sq = R * R + I * I,
                              A#'$ast¯'{args = [R/Sq, -I/Sq]};

%% then plain ones
execute_monadic("+", V) ->  V;
execute_monadic("-", V) -> -1 * V;
execute_monadic("×", V) ->  signum(V);
execute_monadic("÷", V) ->  1 / V;
execute_monadic("|", V) ->  abs(V).

signum(V) when V <  0 -> -1;
signum(V) when V == 0 ->  0;
signum(V) when V >  0 ->  1.

% ×0J0 ↔ 0J0
unit_tensor(_R,_I,Mag) when Mag == 0 -> [0,0];
unit_tensor( R, I,Mag)               -> [R / Mag, I / Mag].

rerank_ravel(Dims, none)  -> [pometo_runtime:get_no_of_elements_from_dims(Dims)];
rerank_ravel(Dims, Float) when is_float(Float) ->
  Insert = trunc(Float),
  if
    Insert > length(Dims) -> throw("Invalid Axis");
    Insert < 0            -> throw("Invalid Axis");
    el/=se                -> {Start, End} = lists:split(Insert, Dims),
                             Start ++ [1] ++ End
  end;
rerank_ravel(Dims, Rank)  -> rer2(Dims, 1, Rank, ?EMPTY_ACCUMULATOR).

rer2([],          _N,  [],          Acc)                  -> lists:reverse(Acc);
rer2(Dims,         N,  [N],         Acc)                  -> lists:reverse(Acc) ++ Dims;
rer2([H1, H2 | T], N,  [N, N2 | R], Acc) when N2 == N + 1 -> rer2([H1 * H2 | T], N + 1, [N2 | R], Acc);
rer2([H1, H2 | T], N,  [R1 | R],    Acc) when R1 >  N     -> rer2([H2 | T],      N + 1, [R1 | R], [H1 | Acc]);
rer2(_Dims,       _N, _Rank,       _Acc)                  -> throw("Invalid Axis").
