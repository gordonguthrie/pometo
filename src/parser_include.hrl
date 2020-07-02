-export([make_err/1]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").

% log/2 is used in debugging the parser and therefore is super useful but also not normally used, so...
-compile([{nowarn_unused_function, [{log, 2}]}]).

% this works
make_stdlib({stdlib, CharNo, _, {Mod, Fn}}, #'$ast¯'{} = A) ->
  #'$ast¯'{do      = {apply_fn, {Mod, Fn}},
           args    = [A],
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()}.

append(#'$ast¯'{do      = #'$shape¯'{dimensions = [D1],
                                     type       = Type1} = R1,
                args    = Args1,
                char_no = CharNo},
       #'$ast¯'{do     = #'$shape¯'{dimensions = [D2],
                                    type       = Type2},
                args   = Args2}) ->
  NewType = match_types(Type1, Type2),
  #'$ast¯'{do      = R1#'$shape¯'{dimensions = [D1 + D2],
                                  type       = NewType},
           args    = Args1 ++ Args2,
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()};
append(#'$ast¯'{do      = #'$shape¯'{dimensions = 0,
                                     type       = Type1} = R1,
                args    = Args1,
                char_no = CharNo},
       #'$ast¯'{do      = #'$shape¯'{dimensions = 0,
                                    type       = Type2},
                args    = Args2}) ->
  NewType = match_types(Type1, Type2),
  #'$ast¯'{do      = R1#'$shape¯'{dimensions = [2],
                                  type       = NewType},
           args    = [Args1, Args2],
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()};
append(#'$ast¯'{do      = #'$shape¯'{dimensions = [D1],
                                     type       = Type1} = R1,
                args    = Args1,
                char_no = CharNo},
       #'$ast¯'{do      = #'$shape¯'{dimensions = 0,
                                    type       = Type2},
                args    = Args2}) ->
  NewType = match_types(Type1, Type2),
  #'$ast¯'{do      = R1#'$shape¯'{dimensions = [D1 + 1],
                                  type       = NewType},
           args    = Args1 ++ [Args2],
           char_no = CharNo,
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

extract_op(monadic, {Type, CharNo, _, Fnname}, Args, Rank) when Type == forwardslash ->
  #'$ast¯'{do      = {monadic_op, [{Fnname, Rank}]},
           args    = Args,
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()};
extract_op(dyadic, {Type, CharNo, _, Fnname}, Args, Rank) when Type == forwardslash ->
  io:format("Fnname is ~p Args is ~p Rank is ~p~n", [Fnname, Args, Rank]),
  #'$ast¯'{do      = {dyadic_op, [{Fnname, Rank}]},
           args    = Args,
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()}.

extract_fn(monadic, {Type, CharNo, _, Fnname}, Args) when Type == scalar_fn orelse
                                                          Type == iota      orelse
                                                          Type == rho       orelse
                                                          Type == ravel ->
  #'$ast¯'{do      = {monadic_fn, [Fnname]},
           args    = Args,
           char_no = CharNo,
           line_no = scope_dictionary:get_line_no()};
extract_fn(dyadic, {Type, CharNo, _, Fnname}, Args) when Type == scalar_fn orelse
                                                         Type == rho ->
  #'$ast¯'{do      = {dyadic_fn, [Fnname]},
           args    = Args,
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

match_types(X,        X)        -> X;
match_types(_,        variable) -> runtime;
match_types(variable, _)        -> runtime;
match_types(number,  boolean)   -> number;
match_types(boolean, number)    -> number;
match_types(_X,      _Y)        -> mixed.

log(X, Label) ->
  ?debugFmt("in " ++ Label ++ " for ~p~n", [X]),
  X.
