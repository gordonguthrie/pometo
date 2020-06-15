%%% -*- mode: erlang -*-

%%% @doc       Lexer for the Pometo 'the little APL' on the BEAM.
%%% @author    gordon@hypernumbers.com
%%% @copyright (C) 2020 Gordon Guthrie

Definitions.

INT      = ([0-9]+)
FLOATDEC = (([0-9]+)?\.[0-9]+)
FLOATSCI = (([0-9]+)?(\.)?[0-9]+(E|e)(\+|\-)?[0-9]+)

TRUE  = (true)
FALSE = (false)
BOOL  = ({TRUE}|{FALSE})

J = (j|J)

VARIABLE = (_?[A-IK-Z][A-Za-z0-9@_]*)

MAYBVARFRAG = ([a-ik-z@_]?)

WHITESPACE = [\000-\s]*

%%" % erlang-mode fix

Rules.

%% Basic data types.
{FLOATDEC}    : {token, {float,         TokenChars, TokenLen, make_float(TokenChars)}}.
{FLOATSCI}    : {token, {float,         TokenChars, TokenLen, make_float(TokenChars)}}.
{INT}         : {token, {int,           TokenChars, TokenLen, to_i(TokenChars)}}.
{BOOL}        : {token, {bool,          TokenChars, TokenLen, string:to_upper(TokenChars) == "TRUE"}}.
{VARIABLE}    : {token, {var,           TokenChars, TokenLen, TokenChars}}.
{MAYBVARFRAG} : {token, {maybe_varfrag, TokenChars, TokenLen, TokenChars}}.

{J} : {token, {j, TokenChars, TokenLen, j}}.

¯ : {token, {unary_negate, TokenChars, TokenLen, "¯"}}.

\+ : {token, {scalar_fn, TokenChars, TokenLen, "+"}}.
-  : {token, {scalar_fn, TokenChars, TokenLen, "-"}}.
×  : {token, {scalar_fn, TokenChars, TokenLen, "×"}}.
÷  : {token, {scalar_fn, TokenChars, TokenLen, "÷"}}.
\| : {token, {scalar_fn, TokenChars, TokenLen, "|"}}.
⌊  : {token, {scalar_fn, TokenChars, TokenLen, "⌊"}}.
⌈  : {token, {scalar_fn, TokenChars, TokenLen, "⌈"}}.
\* : {token, {scalar_fn, TokenChars, TokenLen, "*"}}.
⍟  : {token, {scalar_fn, TokenChars, TokenLen, "⍟"}}.
○  : {token, {scalar_fn, TokenChars, TokenLen, "○"}}.
!  : {token, {scalar_fn, TokenChars, TokenLen, "!"}}.
~  : {token, {scalar_fn, TokenChars, TokenLen, "~"}}.
\? : {token, {scalar_fn, TokenChars, TokenLen, "?"}}.
∊  : {token, {scalar_fn, TokenChars, TokenLen, "∊"}}.
∧  : {token, {scalar_fn, TokenChars, TokenLen, "∧"}}.
∨  : {token, {scalar_fn, TokenChars, TokenLen, "∨"}}.
⍲  : {token, {scalar_fn, TokenChars, TokenLen, "⍲"}}.
⍱  : {token, {scalar_fn, TokenChars, TokenLen, "⍱"}}.
<  : {token, {scalar_fn, TokenChars, TokenLen, "<"}}.
≤  : {token, {scalar_fn, TokenChars, TokenLen, "≤"}}.
=  : {token, {scalar_fn, TokenChars, TokenLen, "="}}.
≥  : {token, {scalar_fn, TokenChars, TokenLen, "≥"}}.
>  : {token, {scalar_fn, TokenChars, TokenLen, ">"}}.
≠  : {token, {scalar_fn, TokenChars, TokenLen, "≠"}}.


← : {token, {let_op, TokenChars, TokenLen, "←"}}. % let is a reserved word in Erlang

⋄ : {token, {seperator, TokenChars, TokenLen, "⋄"}}. % statement separator

{WHITESPACE} : {token, {whitespace, TokenChars, TokenLen, " "}}.

%% comments are ends
⍝  : {end_token, {'$end'}}.
\n : {end_token, {'$end'}}.

%% Anything not covered by rules above is invalid.
.  : {token, {invalid_token, TokenChars, TokenLen, "invalid"}}.

Erlang code.

-include("lexer_include.hrl").
