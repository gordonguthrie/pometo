-module(lexer_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests
basic_int_test_() ->
    Str = "1 2 3 ",
    {ok, Got} = pometo_lexer:get_tokens(Str),
    Exp = [
            {int, 1, 1, "1", 1},
            {int, 1, 3, "2", 2},
            {int, 1, 5, "3", 3}
          ],
    ?_assertEqual(Exp, Got).

basic_float_test_() ->
Str = "1.1 2.2 3.3 ",
{ok, Got} = pometo_lexer:get_tokens(Str),
Exp = [
        {float, 1, 1, "1.1", 1.1},
        {float, 1, 5, "2.2", 2.2},
        {float, 1, 9, "3.3", 3.3}
      ],
?_assertEqual(Exp, Got).

basic_plus_test_() ->
Str = "1.1 2.2 + 3.3 4.4 ",
{ok, Got} = pometo_lexer:get_tokens(Str),
Exp = [
        {float,     1, 1,  "1.1", 1.1},
        {float,     1, 5,  "2.2", 2.2},
        {scalar_fn, 1, 9,  "+",  "+"},
        {float,     1, 11, "3.3", 3.3},
        {float,     1, 15, "4.4", 4.4}
        ],
?_assertEqual(Exp, Got).

basic_let_and_variable_test_() ->
    Str = "MyVariable ← 1 2",
{ok, Got} = pometo_lexer:get_tokens(Str),
Exp = [
        {var,    1, 1,  "MyVariable", "MyVariable"},
        {let_op, 1, 12,  "←", "←"},
        {int,    1, 14, "1", 1},
        {int,    1, 16, "2", 2}
        ],
?_assertEqual(Exp, Got).
