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

VARIABLE = ([A-Z][a-zA-Z_¯∆⍙0-9]*)

WHITESPACE = ([\000-\s]*)

%%" % erlang-mode fix

Rules.

%% Basic data types.
{FLOATDEC} : {token, {float, TokenLine, TokenChars, make_float(TokenChars)}}.
{FLOATSCI} : {token, {float, TokenLine, TokenChars, make_float(TokenChars)}}.
{INT}      : {token, {int,   TokenLine, TokenChars, to_i(TokenChars)}}.
{BOOL}     : {token, {bool,  TokenLine, TokenChars, string:to_upper(TokenChars) == "TRUE"}}.
{VARIABLE} : {token, {var,   TokenLine, TokenChars, TokenChars}}.

¯ : {token, {unary_negate, TokenLine, TokenChars, "¯"}}.

\+ : {token, {scalar_fn, TokenLine, TokenChars, "+"}}.
-  : {token, {scalar_fn, TokenLine, TokenChars, "-"}}.
×  : {token, {scalar_fn, TokenLine, TokenChars, "×"}}.
÷  : {token, {scalar_fn, TokenLine, TokenChars, "÷"}}.

← : {token, {let_op, TokenLine, TokenChars, "←"}}. % let is a reserved word in Erlang

{WHITESPACE} : {token, {whitespace, TokenLine, TokenChars, " "}}.

\n : {end_token, {'$end'}}.

%% Anything not covered by rules above is invalid.
.  : {token, {invalid_token, TokenLine, TokenChars, TokenChars}}.

Erlang code.
-compile([export_all]).
get_tokens(X) ->
    Toks = lex(X),
    post_process(Toks, 1, []).

lex(String) ->
  {ok, Toks, _} = string(String),
  Toks.

post_process([], _N, Acc) ->
 lists:reverse(Acc);
post_process([{whitespace, _, Chars, _} | T], N, Acc) ->
  NewN = N + length(Chars),
  post_process(T, NewN, Acc);
post_process([{Type, LineNo, Chars, Op} | T], N, Acc) ->
  NewN = N + length(Chars),
  NewAcc = {Type, LineNo, N, Chars, Op},
  post_process(T, NewN, [NewAcc | Acc]).

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
