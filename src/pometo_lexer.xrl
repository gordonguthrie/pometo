%% coding: UTF-8\n

%%% -*- mode: erlang -*-

%%% @doc       Lexer for the Pometo 'the little APL' on the BEAM.
%%% @author    gordon@hypernumbers.com
%%% @copyright (C) 2020 Gordon Guthrie

Definitions.

INT = ([0-9]+)
FLOATDEC = (([0-9]+)?\.[0-9]+)
FLOATSCI = (([0-9]+)?(\.)?[0-9]+(E|e)(\+|\-)?[0-9]+)

TRUE = (true)
FALSE = (false)
BOOL = ({TRUE}|{FALSE})

%% is this ugly? yes it is, moving on swiftly
VARIABLE = ([A-Z]([A-Za-z0-9]|[^][\000-\s⌺¨⌶⍫<⍒≤⍋=⌽≥⍉>⊖≠⍟∨⍱∧⍲×!÷⌹?⍵∊⍴~⍨↑↓⍳⍸○⍥*⍣←⍞→⍬⍺⌈⌊∇∘⍤’⌸⎕⌷⍎≡⍕≢#⊢⊣⊂⊆⊃∩∪⊥⊤\|⍝⍪⍀⌿⍠,.;:'"\[{\]}`~!@£$\%\^&\*\(\)-_=\+])*)

WHITESPACE = [\000-\s]*

%%" % erlang-mode fix

Rules.

%% Basic data types.
{FLOATDEC} : {token, {float, TokenLine, TokenChars, TokenLen, make_float(TokenChars)}}.
{FLOATSCI} : {token, {float, TokenLine, TokenChars, TokenLen, make_float(TokenChars)}}.
{INT}      : {token, {int,   TokenLine, TokenChars, TokenLen, to_i(TokenChars)}}.
{BOOL}     : {token, {bool,  TokenLine, TokenChars, TokenLen, string:to_upper(TokenChars) == "TRUE"}}.
{VARIABLE} : {token, {var,   TokenLine, TokenChars, TokenLen, TokenChars}}.

¯ : {token, {unary_negate, TokenLine, TokenChars, TokenLen, "¯"}}.


\+ : {token, {scalar_fn, TokenLine, TokenChars, TokenLen, "+"}}.
-  : {token, {scalar_fn, TokenLine, TokenChars, TokenLen, "-"}}.
×  : {token, {scalar_fn, TokenLine, TokenChars, TokenLen, "×"}}.
÷  : {token, {scalar_fn, TokenLine, TokenChars, TokenLen, "÷"}}.

← : {token, {let_op, TokenLine, TokenChars, TokenLen, "←"}}. % let is a reserved word in Erlang

{WHITESPACE} : {token, {whitespace, TokenLine, TokenChars, TokenLen, " "}}.

%% comments are ends
⍝  : {end_token, {'$end'}}.
\n : {end_token, {'$end'}}.

%% Anything not covered by rules above is invalid.
.  : {token, {invalid_token, TokenLine, TokenChars, TokenLen, "invalid"}}.

Erlang code.
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("parser_records.hrl").

-define(EMPTYACC,       []).
-define(EMPTYERRORLIST, []).

get_tokens(X) ->
    Toks = lex(X),
    post_process(Toks, 1, X, ?EMPTYERRORLIST, ?EMPTYACC).

lex(String) ->
  {ok, Toks, _} = string(String),
  Toks.

post_process(List, _N, _Expr, [],      Acc) when List == [] orelse hd(List) == {'$end'} ->
  {ok, lists:reverse(Acc)};
post_process(List, _N, _Expr, Errors, _Acc) when List == [] orelse hd(List) == {'$end'} ->
  {error, lists:reverse(Errors)};
post_process([{invalid_token, TokenLine, Chars, TokenLen, _}| T], N, Expr, Errors, Acc) ->
  NewError = #error{type = 'SYNTAX ERROR', msg1 = "invalid token", msg2 = Chars, expr = Expr, at_line = TokenLine, at_char = N},
  NewN = N + TokenLen,
  post_process(T, NewN, Expr, [NewError | Errors], Acc);
post_process([{whitespace, _, _Chars, TokenLen, _} | T], N, Expr, Errors, Acc) ->
  NewN = N + TokenLen,
  post_process(T, NewN, Expr, Errors, Acc);
post_process([{Type, LineNo, Chars, TokenLen, Op} | T], N, Expr, Errors, Acc) ->
  NewN = N + TokenLen,
  NewAcc = {Type, LineNo, N, Chars, Op},
  post_process(T, NewN, Expr, Errors, [NewAcc | Acc]).

%% Turn .1/0.1/.1e+10/0.1e+10 into a float.

make_float(TokenChars) ->
    FloatAsStr = case lists:member($., TokenChars) of
                     true  -> TokenChars;
                     false -> add_decimal(TokenChars)
                 end,
    to_f(FloatAsStr).

add_decimal([]) -> ".0";
add_decimal([E | Tail]) when E == $e; E == $E -> ".0e" ++ Tail;
add_decimal([X | Tail]) -> [X | add_decimal(Tail)].


%% String -> integer.
to_i(Str) when is_list(Str) ->
    case to_num(Str) of
        {error, nan} -> {error, nan};
        Num          -> trunc(Num)
    end;
%% Number -> integer.
to_i(Num) when is_number(Num) -> trunc(Num).

%% String -> number.
to_num(Atom) when is_atom(Atom) ->
    to_num(atom_to_list(Atom));
to_num(Str) when is_list(Str)   ->
    Str2 = string:strip(Str),
    try conv_to_int(Str2)
    catch
        exit : _ ->
            try to_f(Str2)
            catch
                error:
                _ -> {error, nan};
                exit:
                _ -> {error, nan}
            end
    end;

to_num(Num) when is_number(Num) ->   Num.

conv_to_int(Str) when is_list(Str) ->
    try list_to_integer(Str)
    catch
        error:_ ->
            case string:tokens(Str, "e+") of
                [Int, Exp] ->
                    I2 = to_num(Int),
                    E2 = to_num(Exp),
                    case {I2, E2} of
                        {{error, nan}, _} -> exit("not integer");
                        {_, {error, nan}} -> exit("not integer");
                        {I3, E3}          -> I3 * math:pow(10, E3)
                    end;
                _          -> exit("not integer")
            end
    end.

%% @doc Convert value to float.
to_f(Str) when is_list(Str) ->
    %
    NStr = re:replace(Str, "^((?:-|\\+)?[0-9]+)(E(?:-|\\+)?[0-9]+)$", %"
                      "\\1.0\\2", [{return, list}]),
    {ok, [Val], []} = io_lib:fread("~f", NStr),
    Val;
to_f(F) when is_float(F) -> F.
