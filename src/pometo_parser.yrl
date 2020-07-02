Nonterminals

Exprs
Expr
Vecs
Vector
Scalar
Let
Value
Var
Dy_and_mon_fns
Mon_fns
Ops
Hybrids
FirstHybrids
LastHybrids

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
open_sq
close_sq
iota
rho
ravel
stdlib
forwardslash
backslash
barredforwardslash
barredbackslash

.

Rootsymbol Exprs.
Endsymbol  '$end'.

Left 50  open_bracket.
Left 50  open_sq.
Left 100 Vector.
Left 200 Expr.
Left 300 Scalar.

Exprs -> Expr                  : ['$1'].
Exprs -> Ops                   : ['$1'].
Exprs -> Exprs seperator Exprs : '$1' ++ '$3'.

Ops ->      LastHybrids  Vecs                 : extract_op(monadic, '$1', ['$2'],       last).
Ops -> Vecs LastHybrids  Vecs                 : extract_op(dyadic,  '$2', ['$1', '$3'], last).
Ops ->      FirstHybrids Vecs                 : extract_op(monadic, '$1', ['$2'],       first).
Ops -> Vecs FirstHybrids Vecs                 : extract_op(dyadic,  '$2', ['$1', '$3'], first).
Ops ->      Hybrids open_sq int close_sq Vecs : extract_op(monadic, '$1', ['$2'],       '$3').
Ops -> Vecs Hybrids open_sq int close_sq Vecs : extract_op(dyadic,  '$2', ['$1', '$6'], '$4').

Expr -> Vecs                     : '$1'.
Expr -> Let                      : '$1'.
Expr -> Vecs scalar_fn      Vecs : extract_fn(dyadic,  '$2', ['$1', '$3']).
Expr -> Vecs Dy_and_mon_fns Vecs : extract_fn(dyadic,  '$2', ['$1', '$3']).
Expr -> Vecs Hybrids        Vecs : extract_fn(dyadic,  '$2', ['$1', '$3']).
Expr ->      scalar_fn      Vecs : extract_fn(monadic, '$1', ['$2']).
Expr ->      Dy_and_mon_fns Vecs : extract_fn(monadic, '$1', ['$2']).
Expr ->      Hybrids        Vecs : extract_fn(monadic, '$1', ['$2']).
Expr ->      Mon_fns        Vecs : extract_fn(monadic, '$1', ['$2']).
Expr ->      stdlib         Vecs : make_stdlib('$1', '$2').

Hybrids -> FirstHybrids : '$1'.
Hybrids -> LastHybrids  : '$1'.

LastHybrids -> forwardslash       : '$1'.
LastHybrids -> backslash          : '$1'.

FirstHybrids  -> barredforwardslash : '$1'.
FirstHybrids  -> barredbackslash    : '$1'.

Dy_and_mon_fns -> rho : '$1'.

Mon_fns -> ravel : '$1'.
Mon_fns -> iota  : '$1'.

Let -> Var let_op Vecs : make_let('$1', '$3').
Let -> Var let_op Expr : make_let('$1', '$3').

Vecs -> Vector      : '$1'.
Vecs -> Vecs Vector : append('$1', '$2').

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
