-export([make_err/1]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").

% log/2 is used in debugging the parser and therefore is super useful but also not normally used, so...
-compile([{nowarn_unused_function, [{log, 2}]}]).

op_to_fn(#'$ast¯'{do      = #'$func¯'{do = Do}   = Func,
                  args    = []}                  = AST1,
         #'$ast¯'{do      = #'$func¯'{do   = [Op],
                                      rank = Rank},
                  line_no = LNo,
                  char_no = CNo}) ->
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

add_rank({Type, CharNo, _, Val},
         #'$ast¯'{do   = #'$shape¯'{dimensions = D},
                  args = Rank}) when is_list(D) ->
  make_fn_ast2(Val, Type, Rank, [], CharNo);
add_rank({Type, CharNo, _, Val},
         {float, _,     _, F}) ->
  make_fn_ast2(Val, Type, F, [], CharNo);
add_rank({Type, CharNo, _, Val}, Rank) when is_atom(Rank) ->
  make_fn_ast2(Val, Type, Rank, [], CharNo);
add_rank({Type, CharNo, _, Val}, #'$ast¯'{do   = #'$shape¯'{dimensions = 0},
                                          args = Rank}) ->
  make_fn_ast2(Val, Type, Rank, [], CharNo).

make_fn_ast({Type, CharNo, _, Val}) ->
  make_fn_ast2(Val, Type, default_rank(Val), [], CharNo).

maybe_merge(#'$ast¯'{do   = #'$func¯'{do   = Fn1,
                                      type = Type1} = Func,
                     args = []}                     = AST1,
            #'$ast¯'{do   = #'$func¯'{do   = Fn2,
                                      type = Type2},
                     args = []}                     = AST2)
  when (Type1 == monadic        orelse
        Type1 == monadic_ranked orelse
        Type1 == ambivalent)    andalso
       (Type2 == monadic        orelse
        Type2 == monadic_ranked orelse
        Type2 == ambivalent) ->
  case {is_primitive_fn_shape_changing(Fn1), is_primitive_fn_shape_changing(Fn2)} of
    {false, false} -> AST1#'$ast¯'{do   = Func#'$func¯'{do = Fn2 ++ Fn1}};
    {_,     _}     -> NewAST2 = make_monadic(AST2),
                      AST1#'$ast¯'{args = [NewAST2]}
  end.

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

% we might have merged up some functions on the RHS, so demerge them...
make_dyadic(#'$ast¯'{do   = #'$func¯'{do   = Dos,
                                      type = Type} = Func,
                     args = Args}                    = FuncAST,
            #'$ast¯'{}                               = LeftAST,
            #'$ast¯'{}                               = RightAST) when length(Dos) > 1         andalso
                                                                      (Type  == dyadic        orelse
                                                                       Type == dyadic_ranked orelse
                                                                       Type == hybrid        orelse
                                                                       Type == ambivalent)   ->
  {LeftDo, RightDo} = unmerge_dos(Dos),
  NewRightAST = case Args of
    []    -> RightAST;
    [Arg] -> descend_arg(Arg, RightAST)
  end,
  Monadic = FuncAST#'$ast¯'{do   = Func#'$func¯'{do   = RightDo,
                                                 type = monadic},
                            args = [NewRightAST]},
  FuncAST#'$ast¯'{do   = Func#'$func¯'{do   = LeftDo,
                                       type = dyadic},
                  args = [LeftAST, Monadic]};
make_dyadic(#'$ast¯'{do   = #'$func¯'{type = Type} = Func,
                     args = Args}                    = FuncAST,
            #'$ast¯'{}                               = LeftAST,
            #'$ast¯'{}                               = RightAST) when Type == dyadic        orelse
                                                                      Type == dyadic_ranked orelse
                                                                      Type == hybrid        orelse
                                                                      Type == ambivalent    ->
  NewRightAST = case Args of
    []    -> RightAST;
    [Arg] -> descend_arg(Arg, RightAST)
  end,
  FuncAST#'$ast¯'{do   = Func#'$func¯'{type = dyadic},
                  args = [LeftAST, NewRightAST]}.

unmerge_dos(Dos) ->
  Len = length(Dos),
  {RightDo, LeftDo} = lists:split(Len - 1, Dos),
  % Dos are arsey-backwards
  {LeftDo, RightDo}.

descend_arg(#'$ast¯'{args = []}     = AST, NewArg) -> AST#'$ast¯'{args = [NewArg]};
descend_arg(#'$ast¯'{args = [AST2]} = AST, NewArg) -> NewArg2 = descend_arg(AST2, NewArg),
                                                      AST#'$ast¯'{args = [NewArg2]}.

make_monadic(#'$ast¯'{do   = #'$func¯'{type = Type} = Func,
                      args = Args}                         = FuncAST,
             #'$ast¯'{}                                  = AST) when Type == monadic        orelse
                                                                     Type == monadic_ranked orelse
                                                                     Type == ambivalent ->
  NewArg = case Args of
    []    -> AST;
    [Arg] -> descend_arg(Arg, AST)
  end,
  FuncAST#'$ast¯'{do   = Func#'$func¯'{type = monadic},
                  args = [NewArg]}.

% this works
make_stdlib({stdlib, CharNo, _, {Mod, Fn}}, #'$ast¯'{} = A) ->
  #'$ast¯'{do      = [{apply_fn, {Mod, Fn}}],
           args    = [A],
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()}.

append(#'$ast¯'{do = #'$shape¯'{} = Shp} = AST1,
        #'$ast¯'{do = #'$shape¯'{}}      = AST2) ->
  {NewType, NewDims, NewArgs} = combine_asts(AST1, AST2),
  AST1#'$ast¯'{do      = Shp#'$shape¯'{dimensions = NewDims,
                                       type       = NewType},
               args    = NewArgs,
               line_no = scope_dictionary:get_line_no()}.

make_scalar({Type, CharNo, _, {R, I}}, complex) when Type == complex_number       orelse
                                                     Type == maybe_complex_number ->
  Arg = #'$ast¯'{do      = complex,
                 args    = [R, I],
                 char_no = CharNo,
                 line_no = scope_dictionary:get_line_no()},
  Shp = basic_shape(CharNo, complex, scalar),
  #'$ast¯'{do         = Shp,
           args       = Arg,
           char_no    = CharNo,
           line_no    = scope_dictionary:get_line_no()};
make_scalar({_Token, CharNo, _, Val}, Type) when Type == number andalso
                                                 (Val == 0      orelse
                                                  Val == 1)     ->
  Shp = basic_shape(CharNo, boolean, scalar),
  #'$ast¯'{do         = Shp,
           args       = Val,
           char_no    = CharNo,
           line_no    = scope_dictionary:get_line_no()};
make_scalar({_Token, CharNo, _, Val}, Type) when Type == number   orelse
                                                 Type == variable ->
  Shp = basic_shape(CharNo, Type, scalar),
  #'$ast¯'{do         = Shp,
           args       = Val,
           char_no    = CharNo,
           line_no    = scope_dictionary:get_line_no()}.

handle_value(Sign, #'$ast¯'{do      = #'$shape¯'{dimensions = 0,
                                                 type       = complex} = Shp,
                            args = #'$ast¯'{do   = complex,
                                            args = [R, I]} = InnerA} = OuterA) ->
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
                            char_no = CharNo} = A) when Type == number  orelse
                                                        Type == boolean ->
  Shp = basic_shape(CharNo, number, scalar),
  SignedVal = case Sign of
    positive ->  Val;
    negative -> -Val
  end,
  A#'$ast¯'{do      = Shp,
            args    = SignedVal,
            line_no = scope_dictionary:get_line_no()}.

make_var({var, CharNo, _, Var}) ->
  Shp = basic_shape(CharNo, variable, scalar),
  #'$ast¯'{do     = Shp,
           args   = #'$var¯'{name    = Var,
                             char_no = CharNo,
                             line_no = scope_dictionary:get_line_no()},
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()}.

make_let(#'$ast¯'{args = #'$var¯'{} = V}, #'$ast¯'{} = Expr) ->
  #'$var¯'{name     = Var,
           char_no  = CharNo} = V,
  B = #{binding => V, results => Expr},
  ok = scope_dictionary:puts({Var, B}),
  #'$ast¯'{do      = 'let',
           args    = [list_to_atom(Var), Expr],
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()}.

make_err({CharNo, pometo_parser, [Error | Body]}) ->
  #error{type    = "SYNTAX ERROR",
         msg1    = Error,
         msg2    = io_lib:format("~ts", [Body]),
         expr    = "",
         at_line = scope_dictionary:get_line_no(),
         at_char = CharNo};
make_err({duplicates, {Var, {B1, B2}}}) ->
  #'$var¯'{char_no = C1,
           line_no = L1} = maps:get(binding, B1),
  #'$var¯'{char_no = C2} = maps:get(binding, B2),
  Msg2 = io_lib:format("was previously assigned on line ~p at char ~p", [L1, C1]),
  #error{type    = "VARIABLE REASSIGNED",
         msg1    = Var,
         msg2    = Msg2,
         expr    = "",
         at_line = scope_dictionary:get_line_no(),
         at_char = C2}.

% enclose a scalar results in a scalar
maybe_enclose_vector({open_bracket, _, _, _},
                      #'$ast¯'{do = #'$shape¯'{dimensions = 0}} = A1) ->
  A1;
maybe_enclose_vector({open_bracket, CharNo, _, _},
                      #'$ast¯'{do = #'$shape¯'{}} = A1) ->
  #'$ast¯'{do      = basic_shape(CharNo, array, scalar),
           args    = A1,
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()}.

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

combine_asts(#'$ast¯'{do      = #'$shape¯'{dimensions = D1,
                                           type       = Type1},
                      args    = Args1},
             #'$ast¯'{do      = #'$shape¯'{dimensions = D2,
                                           type       = Type2},
                      args    = Args2}) ->
  {NewDims, NewArgs} = case {D1, D2} of
              {0,    0}    -> {[2],       [Args1,    Args2]};
              {[N], 0}     -> {[N + 1],   Args1 ++ [Args2]};
              {0,    [N]}  -> {[1 + N],   [Args1 |   Args2]};
              {[N1], [N2]} -> {[N1 + N2],  Args1 ++  Args2}
  end,
  NewType = match_types(Type1, Type2),
  {NewType, NewDims, NewArgs}.

match_types(X,        X)        -> X;
match_types(_,        variable) -> runtime;
match_types(variable, _)        -> runtime;
match_types(number,  boolean)   -> number;
match_types(boolean, number)    -> number;
match_types(_X,      _Y)        -> mixed.

log(X, Label) ->
  ?debugFmt("in " ++ Label ++ " for ~p~n", [X]),
  X.

is_op_shape_changing("/") -> true;
is_op_shape_changing(_)   -> false.

is_primitive_fn_shape_changing(["⍴"]) -> true;
is_primitive_fn_shape_changing(["⍳"]) -> true;
is_primitive_fn_shape_changing([","]) -> true;
is_primitive_fn_shape_changing(_)     -> false.

default_rank({_, _, _, ","})  -> none; % the ravel operator has funky ranking - it takes vectors not scalars
default_rank({_, _, _, "/"})  -> first;
default_rank({_, _, _, "\\"}) -> first;
default_rank({_, _, _, "⌿"})  -> last;
default_rank({_, _, _, "⍀"})  -> last;
default_rank(_)               -> none.

make_monadic(#'$ast¯'{do = #'$func¯'{} = Func} = AST) -> AST#'$ast¯'{do = Func#'$func¯'{type = monadic}}.
