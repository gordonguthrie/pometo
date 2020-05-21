Nonterminals

Expression
Vector
Scalar
Let
Value

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

Let -> var let_op Vector : make_let('$1', '$3', #{}).

Vector -> Vector Scalar : append('$1', '$2').
Vector -> Scalar        : '$1'.

Scalar -> unary_negate Value  : handle_value(negative, '$2').
Scalar -> Value               : handle_value(positive, '$1').

Value -> int   : '$1'.
Value -> float : '$1'.

Erlang code.

-include("parser_include.hrl").