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

\( : {token, {open_bracket,  TokenChars, TokenLen, "("}}.
\) : {token, {close_bracket, TokenChars, TokenLen, ")"}}.
\{ : {token, {open_cy,       TokenChars, TokenLen, "{"}}.
\} : {token, {close_cy,      TokenChars, TokenLen, "}"}}.
\[ : {token, {open_sq,       TokenChars, TokenLen, "["}}.
\] : {token, {close_sq,      TokenChars, TokenLen, "]"}}.

¯ : {token, {unary_negate, TokenChars, TokenLen, "¯"}}.

%% Hybrids can be either functions or operators depending on context
%% * all Hybrids take rank
%% * all Hybrids are monadic operators but dyadic functions

/  : {token, {hybrid, TokenChars, TokenLen, "/"}}.
\\ : {token, {hybrid, TokenChars, TokenLen, "\\"}}.
⌿  : {token, {hybrid, TokenChars, TokenLen, "⌿"}}.
⍀  : {token, {hybrid, TokenChars, TokenLen, "⍀"}}.

%% Implemented Functions that don't take rank

\+ : {token, {ambivalent, TokenChars, TokenLen, "+"}}.
-  : {token, {ambivalent, TokenChars, TokenLen, "-"}}.
×  : {token, {ambivalent, TokenChars, TokenLen, "×"}}.
÷  : {token, {ambivalent, TokenChars, TokenLen, "÷"}}.

⍴ : {token, {ambivalent, TokenChars, TokenLen, "⍴"}}.

⍳ : {token, {monadic, TokenChars, TokenLen, "⍳"}}.

%% Implemented Functions that do take rank
, : {token, {monadic_ranked, TokenChars, TokenLen, ","}}.

%% Not Yet Implemented Functions
%% yeah, yeah bud we will get round to you
\| : {token, {ambivalent, TokenChars, TokenLen, "|"}}.
⌊  : {token, {ambivalent, TokenChars, TokenLen, "⌊"}}.
⌈  : {token, {ambivalent, TokenChars, TokenLen, "⌈"}}.
\* : {token, {ambivalent, TokenChars, TokenLen, "*"}}.
⍟  : {token, {ambivalent, TokenChars, TokenLen, "⍟"}}.
○  : {token, {ambivalent, TokenChars, TokenLen, "○"}}.
!  : {token, {ambivalent, TokenChars, TokenLen, "!"}}.
~  : {token, {ambivalent, TokenChars, TokenLen, "~"}}.
\? : {token, {ambivalent, TokenChars, TokenLen, "?"}}.
∊  : {token, {ambivalent, TokenChars, TokenLen, "∊"}}.

%% yeah, yeah bud we will get round to you
∧  : {token, {dyadic, TokenChars, TokenLen, "∧"}}.
∨  : {token, {dyadic, TokenChars, TokenLen, "∨"}}.
⍲  : {token, {dyadic, TokenChars, TokenLen, "⍲"}}.
⍱  : {token, {dyadic, TokenChars, TokenLen, "⍱"}}.
<  : {token, {dyadic, TokenChars, TokenLen, "<"}}.
≤  : {token, {dyadic, TokenChars, TokenLen, "≤"}}.
=  : {token, {dyadic, TokenChars, TokenLen, "="}}.
≥  : {token, {dyadic, TokenChars, TokenLen, "≥"}}.
>  : {token, {dyadic, TokenChars, TokenLen, ">"}}.
≠  : {token, {dyadic, TokenChars, TokenLen, "≠"}}.

%% Bits and Bobs

← : {token, {let_op, TokenChars, TokenLen, "←"}}. % let is a reserved word in Erlang

⋄ : {token, {seperator, TokenChars, TokenLen, "⋄"}}. % statement separator

%% StdLib Fns

⎕debug            : {token, {stdlib, TokenChars, TokenLen, {pometo_stdlib, debug}}}.
⎕make_indexed     : {token, {stdlib, TokenChars, TokenLen, {pometo_stdlib, make_indexed}}}.
⎕make_lazy        : {token, {stdlib, TokenChars, TokenLen, {pometo_stdlib, make_lazy}}}.
⎕force_indexing   : {token, {stdlib, TokenChars, TokenLen, {pometo_stdlib, force_indexing}}}.
⎕force_unindexing : {token, {stdlib, TokenChars, TokenLen, {pometo_stdlib, force_unindexing}}}.

{WHITESPACE} : {token, {whitespace, TokenChars, TokenLen, " "}}.

%% comments are ends
⍝  : {end_token, {'$end'}}.
\n : {end_token, {'$end'}}.

%% Anything not covered by rules above is invalid.
.  : {token, {invalid_token, TokenChars, TokenLen, "invalid"}}.

Erlang code.

-include("lexer_include.hrl").
