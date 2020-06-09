-export([make_err/1]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").

append(#liffey{op     = #'¯¯⍴¯¯'{dimensions = [D1]} = R1,
              args    = Args1,
              char_no = CharNo},
       #liffey{op     = #'¯¯⍴¯¯'{dimensions = [D2]},
               args   = Args2}) ->
  #liffey{op      = R1#'¯¯⍴¯¯'{dimensions = [D1 + D2]},
          args    = Args1 ++ Args2,
          char_no = CharNo,
          line_no = scope_dictionary:get_line_no()}.

handle_value(Sign, {_, CharNo, _, Val}) ->
  Rho = #'¯¯⍴¯¯'{style      = eager,
                 indexed    = false,
                 dimensions = [1],
                 char_no    = CharNo,
                 line_no    = scope_dictionary:get_line_no()},
  SignedVal = case Sign of
    positive ->  Val;
    negative -> -Val
  end,
  #liffey{op      = Rho,
          args    = [SignedVal],
          char_no = CharNo,
          line_no = scope_dictionary:get_line_no()}.

make_var({var, CharNo, _, Var}) ->
  Rho = #'¯¯⍴¯¯'{style      = eager,
                 indexed    = false,
                 dimensions = [1],
                 char_no    = CharNo,
                 line_no    = scope_dictionary:get_line_no()},
  #liffey{op      = Rho,
          args    = [#var{name    = Var,
                          char_no = CharNo,
                          line_no = scope_dictionary:get_line_no()}],
          char_no = CharNo,
          line_no = scope_dictionary:get_line_no()}.

extract(monadic, {scalar_fn, CharNo, _, Fnname}, Args) ->
  #liffey{op      = {monadic, Fnname},
          args    = Args,
          char_no = CharNo,
          line_no = scope_dictionary:get_line_no()};
extract(dyadic, {scalar_fn, CharNo, _, Fnname}, Args) ->
  #liffey{op      = {dyadic, Fnname},
          args    = Args,
          char_no = CharNo,
          line_no = scope_dictionary:get_line_no()}.

make_let(#liffey{args = [#var{} = V]}, #liffey{} = Expr) ->
  #var{name     = Var,
       char_no  = CharNo} = V,
  B = #{binding => V, results => Expr},
  ok = scope_dictionary:puts({Var, B}),
  #liffey{op      = 'let',
          args    = [list_to_atom(Var), Expr],
          char_no = CharNo,
          line_no = scope_dictionary:get_line_no()}.

log(X, Label) ->
  ?debugFmt("in " ++ Label ++ " for ~p~n", [X]),
  X.

-define(OPENCURLY, 123).
-define(COMMA,     44).

make_err({CharNo, pometo_parser, [Error | Body]}) ->
  #error{type    = "SYNTAX ERROR",
         msg1    = Error,
         msg2    = io_lib:format("~ts", [Body]),
         expr    = "",
         at_line = scope_dictionary:get_line_no(),
         at_char = CharNo};
make_err({duplicates, {Var, {B1, B2}}}) ->
  #var{char_no = C1,
       line_no = L1} = maps:get(binding, B1),
  #var{char_no = C2} = maps:get(binding, B2),
  Msg2 = io_lib:format("was previously assigned on line ~p at char ~p", [L1, C1]),
  #error{type    = "VARIABLE REASSIGNED",
         msg1    = Var,
         msg2    = Msg2,
         expr    = "",
         at_line = scope_dictionary:get_line_no(),
         at_char = C2}.
