-module(lexer_error_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests
basic_failure_test_() ->
    Str = "¯←⍳1 2 3",
    [{error, Got}] = pometo:lex_TEST(Str),
    Exp = "Error\n" ++
          "¯←⍳1 2 3\n" ++
          "--^\n" ++
          "SYNTAX ERROR (invalid token:⍳) on line 1 at character 3\n" ++
          "\n",
    % ?debugFmt("Got ~ts~nExp ~ts~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).