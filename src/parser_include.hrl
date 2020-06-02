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
          args    = [#var{name = Var}],
          char_no = CharNo,
          line_no = scope_dictionary:get_line_no()}.

extract(monadic, {scalar_fn, CharNo, _, Fnname}, Args) ->
  L = #liffey{op      = {monadic, Fnname},
              args    = Args,
              char_no = CharNo,
              line_no = scope_dictionary:get_line_no()},
  {L, #{}};
extract(dyadic, {scalar_fn, CharNo, _, Fnname}, Args) ->
  L = #liffey{op      = {dyadic, Fnname},
              args    = Args,
              char_no = CharNo,
              line_no = scope_dictionary:get_line_no()},
  {L, #{}}.

make_let(#liffey{args = [#var{} = V]}, #liffey{} = Expr) ->
  ?debugFmt("in make_let for ~p~n", [V]),
  #var{name     = Var,
       char_no  = CharNo} = V,
  ok = scope_dictionary:puts({Var, Expr}),
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
         at_char = CharNo}.
