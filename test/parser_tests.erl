-module(parser_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests

basic_plus_test_() ->
Str = "1.1 2.2 + 3.3 4.4 ",
Tokens = pometo_lexer:get_tokens(Str),
{ok, Got} = pometo_parser:parse(Tokens),
Exp = [
        {float,     1, 1,  "1.1", 1.1},
        {float,     1, 5,  "2.2", 2.2},
        {scalar_fn, 1, 9,  "+",  "+"},
        {float,     1, 11, "3.3", 3.3},
        {float,     1, 15, "4.4", 4.4}
        ],
?_assertEqual(Exp, Got).
