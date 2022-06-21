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
TrainFns
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

Exprs -> Expr                  : log(['$1'], "Exprs -> Expr").
Exprs -> Let                   : log(['$1'], "Exprs -> Let ").
Exprs -> Train                 : log(['$1'], "Exprs -> Train").
Exprs -> Exprs seperator Exprs : log('$1' ++ '$3', "Exprs -> Exprs seperator Exprs").

Expr -> Dyadic             : log('$1', "Expr -> Dyadic").
Expr -> Monadic            : log('$1', "Expr -> Monadic ").
Expr -> Associative        : final_check_on_associative('$1').
Expr -> Vecs               : log('$1', "Expr -> Vecs").
Expr -> stdlib Args        : make_stdlib('$1', '$2').
Expr -> stdlib MaybeVector : make_stdlib('$1', '$2').
Expr -> stdlib Associative : make_stdlib('$1', '$2').

Dyadic  -> Args ConsecutiveFns Args : make_dyadic('$2', '$1', '$3').
Monadic ->      ConsecutiveFns Args : make_monadic('$1', '$2').

Associative ->      ConsecutiveFns : make_right_associative('$1').
Associative -> Args ConsecutiveFns : make_right_associative('$1', '$2').

MaybeVector -> Var    Vector : make_maybe_vector('$1', finalise_vector('$2')).
MaybeVector -> Vector Var    : make_maybe_vector(finalise_vector('$1'), '$2').

Args -> Vecs : log('$1', "Args -> Vecs").

Train -> Args open_bracket TrainFns close_bracket Args : make_dyadic_train('$3', '$1', '$5').
Train ->      open_bracket TrainFns close_bracket Args : make_monadic_train('$2', '$4').
Train ->      open_bracket TrainFns close_bracket      : log('$2', "Train -> open_bracket Fns close_bracket").

TrainFns -> Fns Fn     : make_fn_array('$1', '$2').
TrainFns -> Fns Var    : make_fn_array('$1', '$2').
TrainFns -> Fns Vector : make_fn_array('$1', '$2').
TrainFns -> Fns        : log('$1', "TrainFns -> Fns").
TrainFns -> Fn         : log('$1', "TrainFns -> Fn").

% trains require a minimum of 2 consecutive Fns but monadic/dyadic can do it with one
% hence this construction
ConsecutiveFns -> Fns : log('$1', "ConsecutiveFns -> Fns").
ConsecutiveFns -> Fn  : log('$1', "ConsecutiveFns -> Fn").

Fns -> Fn  Fn    : make_fn_array('$1', '$2').
Fns -> Fns Fn    : add_to_fn_array('$1', '$2').

Fn -> PrimitiveFn : log('$1', "Fn -> PrimitiveFn").
Fn -> Rank        : log('$1', "Fn -> Rank").
Fn -> OpFn        : log('$1', "Fn -> OpFn").
Fn -> Var         : log('$1', "Fn -> Var").

OpFn -> PrimitiveFn Op : op_to_fn('$1', '$2').
OpFn -> Rank        Op : op_to_fn('$1', '$2').

PrimitiveFn -> monadic    : make_fn_ast('$1').
PrimitiveFn -> dyadic     : make_fn_ast('$1').
PrimitiveFn -> ambivalent : make_fn_ast('$1').

Op -> OpRanked : log('$1', "Op -> OpRanked").

Rank -> OpRanked                      : log('$1', "Rank -> OpRanked").
Rank -> Ranked                        : add_rank('$1', default_rank('$1')).
Rank -> Ranked open_sq Int   close_sq : add_rank('$1', '$3').
Rank -> Ranked open_sq float close_sq : add_rank('$1', '$3').

Ranked -> monadic_ranked : log('$1', "Ranked -> monadic_ranked ").

OpRanked -> hybrid                      : add_rank('$1', default_rank('$1')).
OpRanked -> hybrid open_sq Int close_sq : add_rank('$1', '$3').

Let -> Var let_op Vecs : make_let('$1', '$3').
Let -> Var let_op Expr : make_let('$1', '$3').
Let -> Var let_op Fns  : make_let_fn('$1', '$3').
Let -> Var let_op Fn   : make_let_fn('$1', '$3').

Vecs -> Vector : finalise_vector('$1').

Scalar -> open_bracket Vector      close_bracket : maybe_enclose_vector('$1', finalise_vector('$2')).
Scalar -> open_bracket MaybeVector close_bracket : make_right_associative('$2').
Scalar -> unary_negate Value                     : handle_value(negative, '$2').
Scalar -> Value                                  : handle_value(positive, '$1').
Scalar -> unary_negate int                       : handle_value(negative, make_scalar('$2', number)).
Scalar -> int                                    : handle_value(positive, make_scalar('$1', number)).

% a vector of ints might be a rank or it might be a vector of ints
Vector -> Scalar        : log('$1', "Vector -> Scalar").
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
