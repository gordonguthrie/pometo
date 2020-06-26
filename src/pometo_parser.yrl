Nonterminals

Expressions
Expression
Vectors
Vector
Scalar
Let
Value
Var
Dy_and_monadics
Monadics

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
iota
rho
ravel
stdlib

.

Rootsymbol Expressions.
Endsymbol  '$end'.

Left 50   open_bracket.
Left  100 Vector.
Left  200 Expression.

Expressions -> Expression                       : ['$1'].
Expressions -> Expressions seperator Expression : '$1' ++ ['$3'].

Expression -> Vectors                         : '$1'.
Expression -> Let                             : '$1'.
Expression -> Vectors scalar_fn       Vectors : extract(dyadic,  '$2', ['$1', '$3']).
Expression -> Vectors Dy_and_monadics Vectors : extract(dyadic,  '$2', ['$1', '$3']).
Expression ->         scalar_fn       Vectors : extract(monadic, '$1', ['$2']).
Expression ->         Dy_and_monadics Vectors : extract(monadic, '$1', ['$2']).
Expression ->         Monadics        Vectors : extract(monadic, '$1', ['$2']).
Expression ->         stdlib          Vectors : make_stdlib('$1', '$2').

Dy_and_monadics -> rho : '$1'.

Monadics -> ravel : '$1'.
Monadics -> iota  : '$1'.

Let -> Var let_op Vectors    : make_let('$1', '$3').
Let -> Var let_op Expression : make_let('$1', '$3').

Vectors -> Vector                 : '$1'.
Vectors -> Vectors Vector         : append('$1', '$2').

Vector -> Scalar        : '$1'.
Vector -> Vector Scalar : append('$1', '$2').

Scalar -> open_bracket Vector close_bracket : maybe_enclose_vector('$1', '$2').
Scalar -> unary_negate Value                : handle_value(negative, '$2').
Scalar -> Value                             : handle_value(positive, '$1').
Scalar -> Var                               : '$1'.

Value -> int                  : make_scalar('$1', number).
Value -> float                : make_scalar('$1', number).
Value -> complex_number       : make_scalar('$1', complex).
Value -> maybe_complex_number : make_scalar('$1', complex).

Var -> var : make_var('$1').

Erlang code.

-include("parser_include.hrl").
