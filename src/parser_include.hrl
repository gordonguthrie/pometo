-export([
          make_err/1,
          resolve_types/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").
-include("runtime_include.hrl").

% log/2 is used in debugging the parser and therefore is super useful but also not normally used, so...
-compile([{nowarn_unused_function, [{log, 2}]}]).

%%%
%%% logging functions
%%%

report_on_dbg(Function, Headnumber, Args) ->
  case os:getenv("DBGPARSER", false) of
    false -> ok;
    _     -> io:format("*************************~n"),
             io:format("in parser include function ~p/~p (func head: ~p)~n", [Function, length(Args), Headnumber]),
             [io:format("argument: ~p: ~p~n", [Name, Val])|| {Name, Val} <- Args],
             io:format("*************************~n~n")
  end.

log(X, Label) ->
  ?debugFmt("in " ++ Label ++ " for ~p~n", [X]),
  X.

%%%
%%% Functions used by the parser
%%%

final_check_on_associative(#'$ast¯'{do      = [{apply_fn, {pometo_runtime, run_right_associative}}],
                                    args    = [#'$ast¯'{do   = #'$shape¯'{type = maybe_func},
                                                        args = Args}],
                                    char_no = CNo,
                                    line_no = LNo} = AST) ->
  report_on_dbg(final_check_on_associative, 1, [{ast, AST}]),
  RHS = hd(lists:reverse(Args)),
  case RHS of
    #'$ast¯'{do = #'$shape¯'{}} -> AST;
    _                           -> pometo_errors:make_right_assoc_syntax_error(LNo, CNo)
  end;
final_check_on_associative(#'$ast¯'{} = AST) ->
  report_on_dbg(final_check_on_associative, 2, [{ast, AST}]),
  AST.

make_maybe_vector(#'$ast¯'{char_no = CNo} = LHS,
                  #'$ast¯'{}              = RHS) ->
  report_on_dbg(make_maybe_vector, 1, [{lhs, LHS}, {rhs, RHS}]),
  Ret = #'$ast¯'{do      = #'$shape¯'{dimensions = [2],
                                      type    = maybe_func,
                                      char_no = CNo,
                                      line_no = scope_dictionary:get_line_no()},
                 args    = [LHS, RHS],
                 char_no = CNo,
                 line_no = scope_dictionary:get_line_no()},
  Ret.

make_right_associative(#'$ast¯'{do      = #'$shape¯'{type = Type1},
                                char_no = CNo}                             = LHS,
                       #'$ast¯'{do      = #'$shape¯'{type = Type2} = Shp2,
                                args    = Args}                            = RHS)
  when (Type1 == number      orelse
        Type1 == boolean     orelse
        Type1 == variable    orelse
        Type1 == runtime)    andalso
       (Type2 == func        orelse
        Type2 == maybe_func) ->
  report_on_dbg(make_right_associative, 1, [{lhs, LHS}, {rhs, RHS}, {args, Args}]),
  Funcs = RHS#'$ast¯'{do   = Shp2#'$shape¯'{type = maybe_func},
                      args = [LHS | Args]},
  Ret = #'$ast¯'{do      = [{apply_fn, {pometo_runtime, run_right_associative}}],
                 args    = [Funcs],
                 char_no = CNo,
                 line_no = scope_dictionary:get_line_no()},
  Ret;
make_right_associative(#'$ast¯'{do      = #'$shape¯'{}} = LHS,
                       #'$ast¯'{do      = #'$func¯'{},
                                char_no = CNo}          = RHS) ->
  report_on_dbg(make_right_associative, 2, [{lhs, LHS}, {rhs, RHS}]),
  AST = #'$ast¯'{do   = #'$shape¯'{type    = maybe_func,
                                   char_no = CNo,
                                   line_no = scope_dictionary:get_line_no()},
                 args = [LHS, RHS]},
  Ret = #'$ast¯'{do      = [{apply_fn, {pometo_runtime, run_right_associative}}],
                 args    = [AST],
                 char_no = CNo,
                 line_no = scope_dictionary:get_line_no()},
  Ret.

make_right_associative(#'$ast¯'{do = #'$func¯'{}} = AST) ->
  report_on_dbg(make_right_associative, 1, [{ast, AST}]),
  AST;
make_right_associative(#'$ast¯'{do      = #'$shape¯'{type = Type},
                                char_no = CNo} = Funcs) when Type == func       orelse
                                                             Type == maybe_func orelse
                                                             Type == variable   ->
  report_on_dbg(make_right_associative, 2, [{funcs, Funcs}]),
  Ret = #'$ast¯'{do      = [{apply_fn, {pometo_runtime, run_right_associative}}],
                 args    = [Funcs],
                 char_no = CNo,
                 line_no = scope_dictionary:get_line_no()},
  Ret.

make_fn_array(#'$ast¯'{do      = Do1,
                       char_no = CNo} = LHS,
              #'$ast¯'{do      = Do2} = RHS) ->
  report_on_dbg(make_fn_array, 1, [{lhs, LHS}, {rhs, RHS}]),
  Type = case {Do1, Do2} of
      {#'$func¯'{}, #'$func¯'{}} -> func;
      {_, _}                     -> maybe_func
  end,
  #'$ast¯'{do      = #'$shape¯'{dimensions = [2],
                                type       = Type,
                                char_no    = CNo,
                                line_no    = scope_dictionary:get_line_no()},
           args    = [LHS, RHS],
           char_no = CNo,
           line_no = scope_dictionary:get_line_no()}.

add_to_fn_array(#'$ast¯'{do   = #'$shape¯'{dimensions = [N],
                                           type       = Type} = Shp,
                         args = Args} = LHS,
                #'$ast¯'{do   = Do1}  = RHS) ->
  report_on_dbg(add_to_fn_array, 1, [{lhs, LHS}, {rhs, RHS}]),
  NewType = case {Do1, Type} of
      {#'$func¯'{},                func} -> func;
      {#'$func¯'{},                 _}   -> maybe_func;
      {#'$shape¯'{type = variable}, _}   -> maybe_func
  end,
  LHS#'$ast¯'{do   = Shp#'$shape¯'{dimensions = [N + 1],
                                   type       = NewType},
              args = Args ++ [RHS]}.

make_monadic_train(Fns, AST) ->
  #'$ast¯'{char_no = CNo} = Fns,
  report_on_dbg(make_monadic_train, 1, [{fns, Fns}, {ast, AST}]),
  Ret = #'$ast¯'{do      = [{apply_fn, {pometo_runtime, run_maybe_monadic_train}}],
                 args    = [Fns, AST],
                 char_no = CNo,
                 line_no = scope_dictionary:get_line_no()},
  Ret.

make_dyadic_train(Fns, LHS, RHS) ->
  report_on_dbg(make_dyadic_train, 1, [{fns, Fns}, {lhs, LHS}, {rhs, RHS}]),
  #'$ast¯'{char_no = CNo} = Fns,
  #'$ast¯'{do      = [{apply_fn, {pometo_runtime, run_maybe_dyadic_train}}],
           args    = [Fns, LHS, RHS],
           char_no = CNo,
           line_no = scope_dictionary:get_line_no()}.

op_to_fn(#'$ast¯'{do      = #'$func¯'{do = Do} = Func,
                  args    = []}                    = AST1,
         #'$ast¯'{do      = #'$func¯'{do   = [Op],
                                      rank = Rank} = AST2,
                  line_no = LNo,
                  char_no = CNo}) ->
  report_on_dbg(op_to_fn, 1, [{ast1, AST1}, {ast2, AST2}, {line_no, LNo}, {char_no, CNo}]),
  IsChanging = is_op_shape_changing(Op),
  NewOp = #'$op¯'{op      = Op,
                  fns     = Do,
                  line_no = LNo,
                  char_no = CNo},
  AST1#'$ast¯'{do = Func#'$func¯'{do             = [NewOp],
                                  type           = ambivalent,
                                  construction   = operator,
                                  rank           = Rank,
                                  shape_changing = IsChanging}}.

add_rank({Type, CharNo, _, Val} = Expr,
         #'$ast¯'{do   = #'$shape¯'{dimensions = D},
                  args = Rank}  = AST) when is_list(D) ->
  report_on_dbg(add_rank, 1, [{expr, Expr}, {ast, AST}]),
  NewRank = extract_rank(Rank, ?EMPTY_ACCUMULATOR),
  make_fn_ast2(Val, Type, NewRank, [], CharNo);
add_rank({Type, CharNo, _, Val} = Expr,
         {float, _,     _, F} = Float) ->
  report_on_dbg(add_rank, 2, [{expr, Expr}, {float, Float}]),
  make_fn_ast2(Val, Type, F, [], CharNo);
add_rank({Type, CharNo, _, Val} = Expr, Rank) when is_atom(Rank) ->
  report_on_dbg(add_rank, 3, [{expr, Expr}, {rank, Rank}]),
  make_fn_ast2(Val, Type, Rank, [], CharNo);
add_rank({Type, CharNo, _, Val} = Expr,
          #'$ast¯'{do   = #'$shape¯'{dimensions = 0},
                                     args = Rank} = AST) ->
  report_on_dbg(add_rank, 4, [{expr, Expr}, {ast, AST}]),
  NewRank = extract_rank(Rank, ?EMPTY_ACCUMULATOR),
  make_fn_ast2(Val, Type, NewRank, [], CharNo).

make_fn_ast({Type, CharNo, _, Val} = Expr) ->
  report_on_dbg(make_fn_ast, 1, [{expr, Expr}]),
  make_fn_ast2(Val, Type, default_rank(Val), [], CharNo).

make_fn_ast2(Fn, Type, Rank, Args, CharNo) ->
  Func = #'$func¯'{do             = [Fn],
                   type           = Type,
                   shape_changing = is_primitive_fn_shape_changing(Fn),
                   rank           = Rank,
                   char_no        = CharNo,
                   line_no        = scope_dictionary:get_line_no()},
  #'$ast¯'{do      = Func,
           args    = Args,
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()}.

make_dyadic(#'$ast¯'{do   = #'$func¯'{type = Type} = Func,
                     args = Args}                  = FuncAST,
            #'$ast¯'{}                             = LeftAST,
            #'$ast¯'{}                             = RightAST) when Type == dyadic        orelse
                                                                    Type == dyadic_ranked orelse
                                                                    Type == hybrid        orelse
                                                                    Type == ambivalent    ->
  report_on_dbg(make_dyadic, 1, [{funcAST, FuncAST}, {leftAST, LeftAST}, {rightAST, RightAST}]),
  NewRightAST = case Args of
    []    -> RightAST;
    [Arg] -> descend_arg(Arg, monadic, [RightAST])
  end,
  FuncAST#'$ast¯'{do   = Func#'$func¯'{type = dyadic},
                  args = [LeftAST, NewRightAST]};
make_dyadic(#'$ast¯'{do   = #'$shape¯'{type = func},
                     args = Args} = Funcs, LHS, RHS) ->
  report_on_dbg(make_dyadic, 2, [{funcs, Funcs}, {lhs, LHS}, {rhs, RHS}]),
  [#'$ast¯'{do = Dyad} | Monads] = Args,
  NewFuncs = Funcs#'$ast¯'{args = Monads},
  NewRHS = make_monadic(NewFuncs, RHS),
  NewAST = NewFuncs#'$ast¯'{do   = Dyad#'$func¯'{type = dyadic},
                            args = [LHS, NewRHS]},
  NewAST;
make_dyadic(#'$ast¯'{do      = #'$shape¯'{type = maybe_func},
                     char_no = CNo} = Funcs, LHS, RHS) ->
  report_on_dbg(make_dyadic, 3, [{funcs, Funcs}, {lhs, LHS}, {rhs, RHS}]),
  #'$ast¯'{do      = [{apply_fn, {pometo_runtime, run_right_associative}}],
           args    = [Funcs, LHS, RHS],
           char_no = CNo,
           line_no = scope_dictionary:get_line_no()};
make_dyadic(#'$ast¯'{do = #'$shape¯'{type = variable}} = FuncAST, LHS, RHS) ->
  report_on_dbg(make_dyadic, 4, [{funcAST, FuncAST}, {lhs, LHS}, {rhs, RHS}]),
  make_dyadic_train(FuncAST, LHS, RHS).

make_monadic(#'$ast¯'{do   = #'$func¯'{type = Type} = Func,
                      args = Args}                       = FuncAST,
             #'$ast¯'{}                                  = RHS) when Type == monadic        orelse
                                                                     Type == monadic_ranked orelse
                                                                     Type == ambivalent ->
  report_on_dbg(make_monadic, 1, [{funcAST, FuncAST}, {rhs, RHS}]),
  NewArg = case Args of
    []    -> RHS;
    [Arg] -> descend_arg(Arg, monadic, [RHS])
  end,
  Ret = FuncAST#'$ast¯'{do   = Func#'$func¯'{type = monadic},
                        args = [NewArg]},
  Ret;
make_monadic(#'$ast¯'{do = #'$shape¯'{type = func}} = Funcs, RHS) ->
  report_on_dbg(make_monadic, 2, [{funcs, Funcs}, {rhs, RHS}]),
  NewFunc = pometo_runtime:make_runtime_right_associative(Funcs),
  Ret = make_monadic(NewFunc, RHS),
  Ret;
make_monadic(#'$ast¯'{do      = #'$shape¯'{type = maybe_func},
                      char_no = CNo} = Funcs, RHS) ->
  report_on_dbg(make_monadic, 3, [{funcs, Funcs}, {rhs, RHS}]),
  Ret = #'$ast¯'{do      = [{apply_fn, {pometo_runtime, run_right_associative}}],
                 args    = [Funcs, RHS],
                 char_no = CNo,
                 line_no = scope_dictionary:get_line_no()},
  Ret;
make_monadic(#'$ast¯'{do = #'$shape¯'{type = variable}} = FuncAST, RHS) ->
  report_on_dbg(make_monadic, 4, [{funcAST, FuncAST}, {rhs, RHS}]),
  Ret = make_monadic_train(FuncAST, RHS),
  Ret.

make_stdlib({stdlib, CharNo, _, {Mod, Fn}} = Expr, #'$ast¯'{} = AST) ->
  report_on_dbg(make_stdlib, 1, [{expr, Expr}, {ast, AST}]),
  #'$ast¯'{do      = [{apply_fn, {Mod, Fn}}],
           args    = [AST],
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()}.


finalise_vector(#'$ast¯'{do   = #'$shape¯'{type = Type}} = AST) when Type == func orelse
                                                                     Type == maybe_func ->
  report_on_dbg(finalise_vector, 1, [{ast, AST}]),
  AST;
finalise_vector(#'$ast¯'{do   = #'$shape¯'{dimensions = 0}} = AST) ->
  report_on_dbg(finalise_vector, 2, [{ast, AST}]),
  AST;
finalise_vector(#'$ast¯'{do   = #'$shape¯'{dimensions = [D1],
                                           type       = unfinalised_vector} = Shp,
                          args = Args1} = AST) ->
  report_on_dbg(finalise_vector, 3, [{ast, AST}]),
  NewArgs = [X || #'$ast¯'{do   = #'$shape¯'{},
                           args = X} <- Args1],
  NewType = finalise_types(Args1),
  AST#'$ast¯'{do   = Shp#'$shape¯'{dimensions = [D1], type = NewType},
              args = NewArgs}.

append_to_vector(#'$ast¯'{do   = #'$shape¯'{dimensions = [D1],
                                            type       = unfinalised_vector} = Shp,
                          args = Args1}                                             = LHS,
                 #'$ast¯'{do   = #'$shape¯'{dimensions = 0,
                                            type       = Type2}}                    = RHS) ->
  report_on_dbg(append_to_vector, 1, [{lhs, LHS}, {rhs, RHS}]),
  NewArg = case Type2 of
    unfinalised_vector -> finalise_vector(RHS);
    _                  -> RHS
  end,
  Ret = LHS#'$ast¯'{do      = Shp#'$shape¯'{dimensions = [D1 + 1],
                                            type       = unfinalised_vector},
                    args    = Args1 ++ [NewArg],
                    line_no = scope_dictionary:get_line_no()},
  Ret;
append_to_vector(#'$ast¯'{do = #'$shape¯'{} = Shp} = LHS,
                 #'$ast¯'{do = #'$shape¯'{}}       = RHS) ->
  report_on_dbg(append_to_vector, 2, [{lhs, LHS}, {rhs, RHS}]),
  Ret = LHS#'$ast¯'{do      = Shp#'$shape¯'{dimensions = [2],
                                            type       = unfinalised_vector},
                    args    = [LHS, RHS],
                    line_no = scope_dictionary:get_line_no()},
  Ret;
append_to_vector(#'$ast¯'{char_no = CNo}                                                = LHS,
                 #'$ast¯'{do   = [{apply_fn, {pometo_runtime, run_right_associative}}]} = RHS) ->
  report_on_dbg(append_to_vector, 3, [{lhs, LHS}, {rhs, RHS}]),
  Ret = #'$ast¯'{do = #'$shape¯'{dimensions = [2],
                                 type = maybe_func,
                                 char_no = CNo,
                                 line_no = scope_dictionary:get_line_no()},
                 args = [LHS, RHS],
                 char_no = CNo,
                 line_no = scope_dictionary:get_line_no()},
  Ret.

make_scalar({Type, CharNo, _, {R, I}} = Expr, complex) when Type == complex_number       orelse
                                                            Type == maybe_complex_number ->
  report_on_dbg(make_scalar, 1, [{expr, Expr}, {complex, complex}]),
  Arg = #'$ast¯'{do      = complex,
                 args    = [R, I],
                 char_no = CharNo,
                 line_no = scope_dictionary:get_line_no()},
  Shp = basic_shape(CharNo, complex, scalar),
  #'$ast¯'{do         = Shp,
           args       = Arg,
           char_no    = CharNo,
           line_no    = scope_dictionary:get_line_no()};
make_scalar({_Token, CharNo, _, Val} = Expr, Type) when Type == number andalso
                                                        (Val == 0      orelse
                                                         Val == 1)     ->
  report_on_dbg(make_scalar, 2, [{expr, Expr}, {type, Type}]),
  Shp = basic_shape(CharNo, boolean, scalar),
  #'$ast¯'{do         = Shp,
           args       = Val,
           char_no    = CharNo,
           line_no    = scope_dictionary:get_line_no()};
make_scalar({_Token, CharNo, _, Val} = Expr, Type) when Type == number   orelse
                                                        Type == variable ->
  report_on_dbg(make_scalar, 3, [{expr, Expr}, {type, Type}]),
  Shp = basic_shape(CharNo, Type, scalar),
  #'$ast¯'{do         = Shp,
           args       = Val,
           char_no    = CharNo,
           line_no    = scope_dictionary:get_line_no()}.

handle_value(Sign, #'$ast¯'{do      = #'$shape¯'{dimensions = 0,
                                                 type       = complex} = Shp,
                            args = #'$ast¯'{do   = complex,
                                            args = [R, I]} = InnerA} = OuterA) ->
  report_on_dbg(handle_value, 1, [{sign, Sign}, {outerA, OuterA}]),
  SignedArgs = case Sign of
    positive -> [ R,  I];
    negative -> [-R, -I]
  end,
  OuterA#'$ast¯'{do      = Shp,
                 args    = InnerA#'$ast¯'{args = SignedArgs},
                 line_no = scope_dictionary:get_line_no()};
handle_value(Sign, #'$ast¯'{do      = #'$shape¯'{type       = Type,
                                                 dimensions = 0},
                            args    = Val,
                            char_no = CharNo} = AST) when Type == number  orelse
                                                          Type == boolean ->
  report_on_dbg(handle_value, 2, [{sign, Sign}, {ast, AST}]),
  Shp = basic_shape(CharNo, number, scalar),
  SignedVal = case Sign of
    positive ->  Val;
    negative -> -Val
  end,
  AST#'$ast¯'{do      = Shp,
              args    = SignedVal,
              line_no = scope_dictionary:get_line_no()}.

make_var({var, CharNo, _, Var} = Expr) ->
  report_on_dbg(make_var, 1, [{expr, Expr}]),
  Shp = basic_shape(CharNo, variable, scalar),
  #'$ast¯'{do     = Shp,
           args   = #'$var¯'{name    = make_varname(Var),
                             char_no = CharNo,
                             line_no = scope_dictionary:get_line_no()},
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()}.

make_let_fn(#'$ast¯'{args = #'$var¯'{}} = AST, #'$ast¯'{do = #'$shape¯'{type = Type}} = RHS)
  when Type == func           orelse
       Type == maybe_func     ->
  report_on_dbg(make_let_fn, 1, [{ast, AST}, {rhs, RHS}]),
  NewExpr = make_defer_evaluation(RHS),
  make_let(AST, NewExpr);
make_let_fn(#'$ast¯'{args = #'$var¯'{}} = AST, #'$ast¯'{do = #'$func¯'{type = Type}} = RHS)
  when Type == monadic        orelse
       Type == monadic_ranked orelse
       Type == dyadic         orelse
       Type == dyadic_ranked  orelse
       Type == ambivalent ->
  report_on_dbg(make_let_fn, 2, [{ast, AST}, {rhs, RHS}]),
  NewExpr = make_defer_evaluation(RHS),
  make_let(AST, NewExpr);
make_let_fn(AST, RHS) ->
  report_on_dbg(make_let_fn, 3, [{ast, AST}, {rhs, RHS}]),
  make_let(AST, RHS).

make_let(#'$ast¯'{args = #'$var¯'{} = V}, #'$ast¯'{} = Expr) ->
  report_on_dbg(make_let, 1, [{expr, Expr}]),
  #'$var¯'{name     = Var,
           char_no  = CharNo} = V,
  B = #{binding => V, results => Expr},
  ok = scope_dictionary:puts({Var, B}),
  #'$ast¯'{do      = 'let_op',
           args    = [list_to_atom(Var), Expr],
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()};
make_let(_, #error{} = Err) ->
  report_on_dbg(make_let, 2, [{err, Err}]),
  Err.

% enclose a scalar results in a scalar
maybe_enclose_vector({open_bracket, CharNo, _, _} = Expr,
                      #'$ast¯'{do      = #'$shape¯'{dimensions = 0}} = AST) ->
  report_on_dbg(maybe_enclose_vector, 1, [{expr, Expr}, {ast, AST}]),
  AST#'$ast¯'{char_no = CharNo,
              line_no = scope_dictionary:get_line_no()};
maybe_enclose_vector({open_bracket, CharNo, _, _} = Expr,
                      #'$ast¯'{do = #'$shape¯'{}} = AST) ->
  report_on_dbg(maybe_enclose_vector, 2, [{expr, Expr}, {ast, AST}]),
  #'$ast¯'{do      = basic_shape(CharNo, array, scalar),
           args    = AST,
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()}.

%%%
%%% Internal Functions
%%%

descend_arg(#'$ast¯'{do   = #'$func¯'{} = Func,
                     args = []}   = AST, Type, Operands) when is_list(Operands) ->
  AST#'$ast¯'{do   = Func#'$func¯'{type = Type},
              args = Operands};
descend_arg(#'$ast¯'{do   = #'$func¯'{},
                     args = #'$var¯'{}} = AST, _Type, _Operands) ->
  AST;
descend_arg(#'$ast¯'{do   = #'$func¯'{},
                     args = Args} = AST, Type, Operands) when is_list(Args) ->
  NewArgs = [descend_arg(X, Type, Operands) || X <- Args],
  AST#'$ast¯'{args = NewArgs};
descend_arg(#'$ast¯'{do   = #'$func¯'{},
                     args = Arg} = AST, Type, Operands) ->
  NewArg = descend_arg(Arg, Type, Operands),
  AST#'$ast¯'{args = NewArg};
descend_arg(#'$ast¯'{} = AST, _Type, _Operands) ->
  AST.

make_varname(Var) -> lists:flatten(Var ++ "_" ++ scope_dictionary:get_current_scope()).

make_defer_evaluation(#'$ast¯'{char_no = CharNo} = AST) ->
  #'$ast¯'{do      = defer_evaluation,
           args    = [AST],
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()}.

make_err({CharNo, pometo_parser, [Error | Body]}) ->
  [NewBody, _] = io_lib:format("~ts~n", [lists:flatten(Body)]),
  Char = get_character_from_body(NewBody),
  #error{type    = "SYNTAX ERROR",
         msg1    = normalise_error_msg(Error),
         msg2    = io_lib:format("~ts", [Char]),
         expr    = "",
         at_line = scope_dictionary:get_line_no(),
         at_char = CharNo};
make_err({duplicates, {Var, {B1, B2}}}) ->
  #'$var¯'{char_no = C1,
           line_no = L1} = maps:get(binding, B1),
  #'$var¯'{char_no = C2} = maps:get(binding, B2),
  Msg2 = io_lib:format("was previously assigned on line ~p at char ~p", [L1, C1]),
  #error{type    = "VARIABLE REASSIGNED",
         msg1    = unpostfix(Var),
         msg2    = Msg2,
         expr    = "",
         at_line = scope_dictionary:get_line_no(),
         at_char = C2}.

normalise_error_msg("syntax error before: ") -> "syntax error before";
normalise_error_msg(X)                       -> X.

get_character_from_body([]) ->
  "";
get_character_from_body(String) ->
    Body = String ++ ".",
    {ok,    Tokens, _} = erl_scan:string(Body),
    {ok,    Parsed}    = erl_parse:parse_exprs(Tokens),
    {value, Tuple, _}  = erl_eval:exprs(Parsed, []),
    Char = get_char_from_tuple(Tuple),
    Char.

get_char_from_tuple({int, _, Int, _})    -> Int;
get_char_from_tuple({_,   _, _,   Char}) -> Char.

extract_rank([], Acc) -> lists:reverse(Acc);
extract_rank([#'$ast¯'{do   = #'$shape¯'{dimensions = 0},
                       args = X} | T], Acc) ->
  extract_rank(T, [X | Acc]);
extract_rank([H | T], Acc) ->
  extract_rank(T, [H | Acc]);
extract_rank(X, ?EMPTY_ACCUMULATOR) ->
  X.


basic_shape(CharNo, Type, array) ->
  #'$shape¯'{indexed    = false,
             dimensions = [1],
             type       = Type,
             char_no    = CharNo,
             line_no    = scope_dictionary:get_line_no()};
basic_shape(CharNo, Type, scalar) ->
  #'$shape¯'{indexed    = false,
             dimensions = 0,
             type       = Type,
             char_no    = CharNo,
             line_no    = scope_dictionary:get_line_no()}.

match_types(X,        X)        -> X;
match_types(_,        variable) -> runtime;
match_types(variable, _)        -> runtime;
match_types(number,  boolean)   -> number;
match_types(boolean, number)    -> number;
match_types(_X,      _Y)        -> mixed.

is_op_shape_changing("/") -> true;
is_op_shape_changing(_)   -> false.

is_primitive_fn_shape_changing("⍴") -> true;
is_primitive_fn_shape_changing("⍳") -> true;
is_primitive_fn_shape_changing(",") -> true;
is_primitive_fn_shape_changing(_)   -> false.

default_rank({_, _, _, ","})  -> none; % the ravel function has funky ranking - it takes vectors not scalars
default_rank({_, _, _, "/"})  -> first;
default_rank({_, _, _, "\\"}) -> first;
default_rank({_, _, _, "⌿"})  -> last;
default_rank({_, _, _, "⍀"})  -> last;
default_rank(_)               -> none.

finalise_types([#'$ast¯'{do = #'$shape¯'{type = Type}}]) ->
  Type;
finalise_types([#'$ast¯'{do = #'$shape¯'{type = Type}} |
               [#'$ast¯'{do = #'$shape¯'{type = Type}} | _] = T]) ->
  finalise_types(T);
finalise_types(_) ->
  mixed.

resolve_types([H | T]) ->
  resolve_t2(T, H).

resolve_t2([],      Type) -> Type;
resolve_t2([H | T], Type) -> resolve_t2(T, match_types(H, Type)).