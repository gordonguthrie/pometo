-module(format_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests

basic_one_line_format_test_() ->
    Str = "MyVariable ← 1 2 3 4 5",
         %0123456789012345678901234567890123456789
    Got = pometo:interpret_TEST(Str),
    Exp = "1 2 3 4 5",
    % ?debugFmt("in basic_one_line_format_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_nested_format_test_() ->
    Str = "MyVariable ← 1 2 3 4 5 (1 2 3 4 5)",
    Got = pometo:interpret_TEST(Str),
    Exp = "1 2 3 4 5 " ++
          " 1 2 3 4 5",
    % ?debugFmt("in basic_two_line_format_test_~nGot ~ts~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_one_line_bust_measure_format_test_() ->
    Str = "MyVariable ← 1 2 3 4 5 6 7 8 9 0" ++
    	              " 1 2 3 4 5 6 7 8 9 0" ++
    	              " 1 2 3 4 5 6 7 8 9 0" ++
    	              " 1 2 3 4 5 6 7 8 9 0\n" ++
    	   "MyVariable + 1",
    Got = pometo:interpret_TEST(Str),
    Exp = "2 3 4 5 6 7 8 9 10 "   ++
    	  "1 2 3 4 5 6 7 8 9 10 " ++
    	  "1 2 3 4 5 6 7 8 9 10 " ++
    	  "1 2 3 4 5 ...",
    % ?debugFmt("in basic_one_line_bust_measure_format_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_nested_bust_measure_format_test_() ->
    Str = "MyVariable ← (1 2 3 4 5 6 7 8 9 0"  ++
    	              " 1 2 3 4 5 6 7 8 9 0"   ++
    	              " 1 2 3 4 5 6 7 8 9 0"   ++
    	              " 1 2 3 4 5 6 7 8 9 0) " ++
                      " (1 2 3 4 5 6 7 8 9 0"  ++
                      " 1 2 3 4 5 6 7 8 9 0"   ++
                      " 1 2 3 4 5 6 7 8 9 0"   ++
                      " 1 2 3 4 5 6 7 8 9 0) ",
         "MyVariable + 0",
    Got = pometo:interpret_TEST(Str),
    Exp = " 1 2 3 4 5 6 7 8 9 0 " ++
    	  "1 2 3 4 5 6 7 8 9 0 " ++
    	  "1 2 3 4 5 6 7 8 9 0 " ++
    	  "1 2 3 4 5 ...",
    % ?debugFmt("in basic_two_line_bust_measure_format_test_~nGot ~ts~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).
