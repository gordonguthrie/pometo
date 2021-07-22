-module(index_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("runtime_include.hrl").

%%%
%%% Test the index builder
%%%

edge_case_test_() ->
  Dims = [2],
  Axes = pometo_runtime:make_axes(Dims),
  Counts = [
              {1, #{1 => 1}},
              {1, #{1 => 2}}
            ],
  Got = [pometo_runtime:make_index_from_count(C, Axes) || C <- Counts],
  Exp = [1, 2],
  ?_assertEqual(Exp, Got).

basic_index_test_() ->
  X = 1,
  Y = 2,
  Z = 3,
  {Exp, Got} = helper3(X, Y, Z),
  % ?debugFmt("in basic_index_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
  ?_assertEqual(Exp, Got).

basic_index_II_test_() ->
  X = 2,
  Y = 2,
  Z = 3,
  {Exp, Got} = helper3(X, Y, Z),
  % ?debugFmt("in basic_index_test_II_~nGot ~p~nExp ~p~n", [Got, Exp]),
  ?_assertEqual(Exp, Got).

basic_index_III_test_() ->
  X = 7,
  Y = 4,
  Z = 6,
  {Exp, Got} = helper3(X, Y, Z),
  % ?debugFmt("in basic_index_test_II_~nGot ~p~nExp ~p~n", [Got, Exp]),
  ?_assertEqual(Exp, Got).

banger_index_test_() ->
  X = 7,
  Y = 4,
  Z = 6,
  XX = 9,
  YY = 12,
  ZZ = 1,
  {Exp, Got} = helper6(X, Y, Z, XX, YY, ZZ),
  % ?debugFmt("in basic_index_test_II_~nGot ~p~nExp ~p~n", [Got, Exp]),
  ?_assertEqual(Exp, Got).


%%
%% Out of Bounds Test
%%

out_of_bounds_I_test_() ->
  Dims = [1, 2, 3],
  Axes = pometo_runtime:make_axes(Dims),
  Count = {3, #{1 => 0, 2 => 1, 3 => 1}},
  Got = pometo_runtime:make_index_from_count(Count, Axes),
  Exp = out_of_bounds,
  ?_assertEqual(Exp, Got).

out_of_bounds_II_test_() ->
  Dims = [1, 2, 3],
  Axes = pometo_runtime:make_axes(Dims),
  Count = {3, #{1 => 1, 2 => 5, 3 => 1}},
  Got = pometo_runtime:make_index_from_count(Count, Axes),
  Exp = out_of_bounds,
  ?_assertEqual(Exp, Got).

% why yes caller it did fail first time I called it
% I am not as green as I am cabbage looking
out_of_bounds_impossible_ha_ha_test_() ->
  Dims = [1, 2, 3],
  Axes = pometo_runtime:make_axes(Dims),
  Count = {3, #{1 => 1, 2 => -7, 3 => 1}},
  Got = pometo_runtime:make_index_from_count(Count, Axes),
  Exp = out_of_bounds,
  ?_assertEqual(Exp, Got).

helper3(X, Y, Z) ->
  Dims = [X, Y, Z],
  Axes = pometo_runtime:make_axes(Dims),
  Counts = [{3, #{1 => A, 2 => B, 3 => C}} || A <- lists:seq(1, X),
                                              B <- lists:seq(1, Y),
                                              C <- lists:seq(1, Z)],
  Got = [pometo_runtime:make_index_from_count(C, Axes) || C <- Counts],
  Exp = lists:seq(1, X * Y * Z),
  {Exp, Got}.

helper6(X, Y, Z, XX, YY, ZZ) ->
  Dims = [X, Y, Z, XX, YY, ZZ],
  Axes = pometo_runtime:make_axes(Dims),
  Counts = [{6, #{1 => A, 2 => B, 3 => C, 4 => D, 5 => E, 6 => F}} || A <- lists:seq(1, X),
                                                                      B <- lists:seq(1, Y),
                                                                      C <- lists:seq(1, Z),
                                                                      D <- lists:seq(1, XX),
                                                                      E <- lists:seq(1, YY),
                                                                      F <- lists:seq(1, ZZ)],
  Got = [pometo_runtime:make_index_from_count(C, Axes) || C <- Counts],
  Exp = lists:seq(1, X * Y * Z * XX * YY * ZZ),
  {Exp, Got}.