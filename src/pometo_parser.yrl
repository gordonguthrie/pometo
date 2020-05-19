Nonterminals

Expression
Vector
Scalar
Let

.

Terminals

scalar_fn
let_op
var
int
float
unary_negate

.

Rootsymbol Expression.
Endsymbol  '$end'.

Expression -> Let                     : '$1'.
Expression -> Vector scalar_fn Vector : extract(dyadic, '$2', ['$1', '$3']).
Expression -> scalar_fn Vector        : extract(monadic, '$1', ['$2']).

Let -> var let_op Vector : make_let('$1', '$3').

Vector -> Vector Scalar : append_scalar('$1', '$2').
Vector -> Scalar        : '$1'.

Scalar -> unary_negate int   : tag_scalar(int,   negative, '$2').
Scalar -> unary_negate float : tag_scalar(float, negative, '$2').

Scalar -> int                : tag_scalar(int,   positive, '$1').
Scalar -> float              : tag_scalar(float, positive, '$1').

Erlang code.

-include("parser_include.hrl").