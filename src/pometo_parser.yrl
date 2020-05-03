Nonterminals

Expression
Vector
Scalar

.

Terminals

integer
float
scalar_fn

.

Rootsymbol Expression.
Endsymbol  '$end'.

Expression -> Vector scalar_fn Vector : {'$2', ['$1', '$3']}.
Expression -> scalar_fn Vector        : {'$1', ['$2']}.

Vector -> Vector Scalar : append_scalar('$1', '$2').
Vector -> Scalar        : '$1'.

Scalar -> integer       : tag_scalar(integer, '$1').
Scalar -> float         : tag_scalar(float,   '$1').

Erlang code.

-include("parser_include.hrl").