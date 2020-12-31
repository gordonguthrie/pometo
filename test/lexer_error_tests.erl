-module(lexer_error_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests
basic_lexer_failure_test_() ->
    Str = "¯←😃1 2 3",
    [{error, Got}] = pometo:lex_TEST(Str),
    Exp = "\n\nError\n" ++
          "¯←😃1 2 3\n" ++
          "--^\n" ++
          "SYNTAX ERROR [invalid token: 😃 ] on line 1 at character 3",
    % ?debugFmt("in basic_lexer_failure_test_~nGot ~ts~nExp ~ts~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).