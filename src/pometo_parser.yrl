Nonterminals

Exprs
Expr
Vecs
Vector
Scalar
Let
Value
Var
PrimitiveFn
Fn
Fns
Dyadic
Monadic
Associative
MaybeVector
Int
Rank
Ranked
Args
Op
OpRanked
OpFn
ConsecutiveFns
Train

.

Terminals

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
ambivalent
dyadic
monadic
monadic_ranked
hybrid
stdlib

.

Rootsymbol Exprs.
Endsymbol  '$end'.

Right    10 Rank.
Right    11 ConsecutiveFns.
Right    12 Vecs.
Right    20 Vector.
Right    30 Expr.
Right    40 Args.
Right    50 Scalar.
Right    70 Monadic.
Right    80 Dyadic.
Right    85 Fn.
Right    90 Fns.
Right   110 OpFn.
Right   120 OpRanked.
Right   130 open_bracket.
Right   140 open_sq.
Right   996 MaybeVector.
Right   997 seperator.
Right   998 Let.
Right   999 Exprs.

Exprs -> Expr                  : ['$1'].
Exprs -> Let                   : ['$1'].
Exprs -> Train                 : ['$1'].
Exprs -> Exprs seperator Exprs : '$1' ++ '$3'.

%Expr -> Expr ConsecutiveFns Expr : make_dyadic('$2', '$1', '$3').
%Expr ->      ConsecutiveFns Expr : make_monadic('$1', '$2').
%Expr -> Expr ConsecutiveFns Args : make_dyadic('$2', '$1', '$3').
%Expr ->      ConsecutiveFns Args : make_monadic('$1', '$2').
Expr -> Dyadic      : '$1'.
Expr -> Monadic     : '$1'.
Expr -> Associative : final_check_on_associative('$1').
Expr -> Vecs        : '$1'.
Expr -> stdlib Args        : make_stdlib('$1', '$2').
Expr -> stdlib Associative : make_stdlib('$1', '$2').

Dyadic  -> Args ConsecutiveFns Args : make_dyadic('$2', '$1', '$3').
Monadic ->      ConsecutiveFns Args : make_monadic('$1', '$2').

Associative ->      ConsecutiveFns : make_right_associative('$1').
Associative -> Args ConsecutiveFns : make_right_associative('$1', '$2').

MaybeVector -> Var    Vector : make_maybe_vector('$1', finalise_vector('$2')).
MaybeVector -> Vector Var    : make_maybe_vector(finalise_vector('$1'), '$2').

Args -> Vecs : '$1'.

Train -> Args open_bracket Fns close_bracket Args : make_dyadic_train('$3', '$1', '$5').
Train ->      open_bracket Fns close_bracket Args : make_monadic_train('$2', '$4').
Train ->      open_bracket Fns close_bracket      : '$2'.

% trains require a minimum of 2 consecutive Fns but monadic/dyadic can do it with one
% hence this construction
ConsecutiveFns -> Fns : '$1'.
ConsecutiveFns -> Fn  : '$1'.

Fns -> Fn  Fn : make_fn_array('$1', '$2').
Fns -> Fns Fn : add_to_fn_array('$1', '$2').

Fn -> PrimitiveFn : '$1'.
Fn -> Rank        : '$1'.
Fn -> OpFn        : '$1'.
Fn -> Var         : '$1'.

OpFn -> PrimitiveFn Op : op_to_fn('$1', '$2').
OpFn -> Rank        Op : op_to_fn('$1', '$2').

PrimitiveFn -> monadic    : make_fn_ast('$1').
PrimitiveFn -> dyadic     : make_fn_ast('$1').
PrimitiveFn -> ambivalent : make_fn_ast('$1').

Op -> OpRanked : '$1'.

Rank -> OpRanked                      : '$1'.
Rank -> Ranked                        : add_rank('$1', default_rank('$1')).
Rank -> Ranked open_sq Int   close_sq : add_rank('$1', '$3').
Rank -> Ranked open_sq float close_sq : add_rank('$1', '$3').

Ranked -> monadic_ranked : '$1'.

OpRanked -> hybrid                      : add_rank('$1', default_rank('$1')).
OpRanked -> hybrid open_sq Int close_sq : add_rank('$1', '$3').

Let -> Var let_op Vecs : make_let('$1', '$3').
Let -> Var let_op Expr : make_let('$1', '$3').
Let -> Var let_op Fns  : make_let_fn('$1', '$3').
Let -> Var let_op Fn   : make_let_fn('$1', '$3').

Vecs -> Vector      : finalise_vector('$1').

Scalar -> open_bracket Vector      close_bracket : maybe_enclose_vector('$1', finalise_vector('$2')).
Scalar -> open_bracket MaybeVector close_bracket : make_right_associative('$2').
Scalar -> unary_negate Value                     : handle_value(negative, '$2').
Scalar -> Value                                  : handle_value(positive, '$1').
Scalar -> unary_negate int                       : handle_value(negative, make_scalar('$2', number)).
Scalar -> int                                    : handle_value(positive, make_scalar('$1', number)).

% a vector of ints might be a rank or it might be a vector of ints
Vector -> Scalar        : '$1'.
Vector -> Vector Scalar : append_to_vector('$1', '$2').
Vector -> Vector Int    : append_to_vector('$1', '$2').

Value -> float                : make_scalar('$1', number).
Value -> complex_number       : make_scalar('$1', complex).
Value -> maybe_complex_number : make_scalar('$1', complex).

Int -> unary_negate int : handle_value(negative, make_scalar('$2', number)).
Int -> int              : handle_value(positive, make_scalar('$1', number)).
Int -> Int int          : append_to_vector('$1', make_scalar('$2', number)).

Var -> var : make_var('$1').

Erlang code.

% the grammer of APL is not resolvable at write time
% some parsing decisions are delayed until runtime
% and in these cases the runtime has to perform AST transforms that are properly
% defined in the parser
%
% These exports are how they do them
-export([descend_arg/3,
                 make_monadic_train/2]).

-include("parser_include.hrl").
