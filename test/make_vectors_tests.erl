-module(make_vectors_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").

-define(shp(Dim),         #'$shape¯'{dimensions = Dim}).


-define(scalar(Val), #'$ast¯'{do   = #'$shape¯'{dimensions = 0},
                              args = Val}).

-define(vector(Dim, Vals), #'$ast¯'{do   = #'$shape¯'{dimensions = [Dim]},
                                    args = Vals}).

-define(runtime_vector(Dim, Vals), #'$ast¯'{do   = #'$shape¯'{dimensions = [Dim],
                                                              type       = runtime},
                                            args = Vals}).

-define(array(Dims, Vals), #'$ast¯'{do   = #'$shape¯'{dimensions = Dims},
                                    args = Vals}).


-define(maybe_func(Dims, Vals), #'$ast¯'{do   = #'$shape¯'{dimensions = Dims,
                                                           type       = maybe_func},
                                         args = Vals}).

% tests all test the resolution of chains of values such as you would get by typing:
% A ← 1
% B ← 2 3
% A B
%
% Resulting in:
%   -------
% 1 | 2 3 |
%   -------
%
% the point is that these are data structures that are resolved at runtime and not at parse time
% so be careful when checking the tests

%% Tests

%% two AST tests

scalar_plus_vector_test_() ->
  A   = ?maybe_func([1], ?scalar(1)),
  B   = ?vector(2, [2, 3]),
  Got = pometo_runtime:make_vector_TEST(A, B),
  Exp = ?runtime_vector(2, [1, B]),
  % ?debugFmt(" in scalar_plus_vector_test_ ~nExp:~n~p~nGot:~n~p~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

vector_plus_scalar_test_() ->
  A   = ?scalar(1),
  B   = ?vector(2, [2, 3]),
  Got = pometo_runtime:make_vector_TEST(?maybe_func([1], B), A),
  Exp = ?runtime_vector(2, [B, 1]),
  % ?debugFmt(" in vector_plus_scalar_test_ ~nExp:~n~p~nGot:~n~p~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

basic_vector_test_() ->
  A   = ?scalar(1),
  Got = pometo_runtime:make_vector_TEST(?maybe_func([1], A), A),
  Exp = ?runtime_vector(2, [1, 1]),
  % ?debugFmt(" in basic_vector_test_ ~nExp:~n~p~nGot:~n~p~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

basic_vector_II_test_() ->
  A   = ?scalar(1),
  B   = ?scalar(99),
  C   = ?scalar(888),
  Got = pometo_runtime:make_vector_TEST(?maybe_func([3], [A, B, B]), C),
  Exp = ?runtime_vector(4, [1, 99, 99, 888]),
  % ?debugFmt(" in basic_vector_II_test_ ~nExp:~n~p~nGot:~n~p~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

basic_2_vectors_test_() ->
  A   = ?vector(3, [1, 2, 3]),
  B   = ?vector(3, [9, 8, 7]),
  Got = pometo_runtime:make_vector_TEST(?maybe_func([1], A), B),
  Exp = ?runtime_vector(2, [A, B]),
  % ?debugFmt(" in basic_2_vectors_test_ ~nExp:~n~p~nGot:~n~p~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

%% Lists of AST tests

scalars_test_() ->
  A   = ?scalar(1),
  B   = ?scalar(2),
  C   = ?scalar(3),
  Got = pometo_runtime:make_vector_TEST([A, B, C]),
  Exp = ?runtime_vector(3, [1, 2, 3]),
  % ?debugFmt(" in scalars_test_ ~nExp:~n~p~nGot:~n~p~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).
