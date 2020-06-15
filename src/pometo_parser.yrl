Nonterminals

Expressions
Expression
Vectors
Vector
Scalar
Let
Value
Var

.

Terminals

scalar_fn
let_op
var
int
float
unary_negate
seperator
complex_number
maybe_complex_number
open_bracket
close_bracket

.

Rootsymbol Expressions.
Endsymbol  '$end'.

Expressions -> Expression                                        : ['$1'].
Expressions -> Expressions seperator    Expression               : '$1' ++ ['$3'].

Expression -> Vectors                   : '$1'.
Expression -> Let                       : '$1'.
Expression -> Vectors scalar_fn Vectors : extract(dyadic,  '$2', ['$1', '$3']).
Expression -> scalar_fn Vectors         : extract(monadic, '$1', ['$2']).

Var -> var : make_var('$1').

Let -> Var let_op Vectors : make_let('$1', '$3').

Vectors -> Vector                                    : '$1'.
Vectors -> Vectors open_bracket Vector close_bracket : enclose_vector('$1', '$2', '$3').
Vectors ->         open_bracket Vector close_bracket : enclose_vector('$1', '$2').

Vector -> Vector Scalar : append('$1', '$2').
Vector -> Scalar        : '$1'.
Vector -> Var           : '$1'.

Scalar -> unary_negate Value  : handle_value(negative, '$2').
Scalar -> Value               : handle_value(positive, '$1').

Value -> int                  : '$1'.
Value -> float                : '$1'.
Value -> complex_number       : make_complex('$1').
Value -> maybe_complex_number : make_complex('$1').


Erlang code.

-include("parser_include.hrl").
