-module(interpreter_complex_no_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests

basic_one_line_complex_no_test_() ->
    Str = "MyVariable ← 1J3\nMyVariable + 2",
         %0123456789012345678901234567890123456789
    Got = pometo:interpret_TEST(Str),
    Exp = "3J3",
    % ?debugFmt("in basic_one_line_format_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).