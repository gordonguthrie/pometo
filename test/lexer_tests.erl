-module(lexer_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests
basic_int_lexer_test_() ->
    Str = "1 2 3 ",
    [{ok, Got}] = pometo:lex_TEST(Str),
    Exp = [
            {int, 1, "1", 1},
            {int, 3, "2", 2},
            {int, 5, "3", 3}
          ],
    % ?debugFmt("in basic_int_lexer_test_~nGot ~ts~nExp ~ts~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_float_lexer_test_() ->
    Str = "1.1 2.2 3.3 ",
    [{ok, Got}] = pometo:lex_TEST(Str),
    Exp = [
            {float, 1, "1.1", 1.1},
            {float, 5, "2.2", 2.2},
            {float, 9, "3.3", 3.3}
        ],
    % ?debugFmt("in basic_float_lexer_test_~nGot ~ts~nExp ~ts~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_plus_lexer_test_() ->
    Str = "1.1 2.2 + 3.3 4.4 ",
    [{ok, Got}] = pometo:lex_TEST(Str),
    Exp = [
            {float,     1,  "1.1", 1.1},
            {float,     5,  "2.2", 2.2},
            {scalar_fn, 9,  "+",  "+"},
            {float,     11, "3.3", 3.3},
            {float,     15, "4.4", 4.4}
            ],
    % ?debugFmt("in basic_plus_lexer_test_~nGot ~ts~nExp ~ts~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_let_and_variable_lexer_test_() ->
    Str = "MyVariable ← 1 2",
    [{ok, Got}] = pometo:lex_TEST(Str),
    Exp = [
            {var,    1,  "MyVariable", "MyVariable"},
            {let_op, 12,  "←", "←"},
            {int,    14, "1", 1},
            {int,    16, "2", 2}
            ],
    % ?debugFmt("in basic_let_and_variable_lexer_test_~nGot ~ts~nExp ~ts~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).
