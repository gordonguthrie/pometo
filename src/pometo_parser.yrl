Nonterminals

Expression
Vector
Scalar
Let

.

Terminals

int
float
scalar_fn
let_op
var

.

Rootsymbol Expression.
Endsymbol  '$end'.

Expression -> Let                     : '$1'.
Expression -> Vector scalar_fn Vector : extract(dyadic, '$2', ['$1', '$3']).
Expression -> scalar_fn Vector        : extract(monadic, '$1', ['$2']).

Let -> var let_op Vector : make_let('$1', '$3').

Vector -> Vector Scalar : append_scalar('$1', '$2').
Vector -> Scalar        : '$1'.

Scalar -> int   : tag_scalar(int,   '$1').
Scalar -> float : tag_scalar(float, '$1').

Erlang code.

-include("parser_include.hrl").