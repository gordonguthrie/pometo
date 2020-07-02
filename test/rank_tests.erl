-module(rank_tests).


-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("parser_records.hrl").
-include("runtime_include.hrl").

%%%
%%% Test the fragment builder
%%%

basic_rank_scalar_test_() ->
    D1 = 1,
    D2 = 1,
    Got = pometo_runtime:check_rank(D1, D2),
    Exp = notvectors,
    % ?debugFmt("in basic_rank_scalar_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_rank_vector_test_() ->
    D1 = [1],
    D2 = [1],
    Got = pometo_runtime:check_rank(D1, D2),
    Exp = identical,
    % ?debugFmt("in basic_rank_vector_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_rank_vector_fail_I_test_() ->
    D1 = [1, 2],
    D2 = [1, 4],
    Got = pometo_runtime:check_rank(D1, D2),
    Exp = invalid_rank,
    % ?debugFmt("in basic_rank_vector_fail_I_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_rank_vector_fail_II_test_() ->
    D1 = [1, 2],
    D2 = [1, 3, 4],
    Got = pometo_runtime:check_rank(D1, D2),
    Exp = invalid_rank,
    % ?debugFmt("in basic_rank_vector_fail_II_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_rank_unsized_vector_I_test_() ->
    D1 = unsized_vector,
    D2 = [1, 4],
    Got = pometo_runtime:check_rank(D1, D2),
    Exp = unsized_vector,
    % ?debugFmt("in basic_rank_unsized_vector_I_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_rank_unsized_vector_II_test_() ->
    D1 = [1, 4],
    D2 = unsized_vector,
    Got = pometo_runtime:check_rank(D1, D2),
    Exp = unsized_vector,
    % ?debugFmt("in basic_rank_unsized_vector_II_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_rank_unsized_vector_III_test_() ->
    D1 = unsized_vector,
    D2 = unsized_vector,
    Got = pometo_runtime:check_rank(D1, D2),
    Exp = unsized_vector,
    % ?debugFmt("in basic_rank_unsized_vector_III_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_rank_subrank_vector_test_() ->
    D1 = [1, 2, 3],
    D2 = [1, 2],
    Got = pometo_runtime:check_rank(D1, D2),
    Exp = subrank,
    % ?debugFmt("in basic_rank_subrank_vector_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_rank_superank_vector_test_() ->
    D1 = [1, 2],
    D2 = [1, 2, 3],
    Got = pometo_runtime:check_rank(D1, D2),
    Exp = superrank,
    % ?debugFmt("in basic_rank_superank_vector_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).