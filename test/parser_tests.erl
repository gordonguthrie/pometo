-module(parser_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests

basic_plus_test_() ->
Str = "1.1 2.2 + 3.3 4.4 ",
Tokens = pometo_lexer:get_tokens(Str),
{ok, Got} = pometo_parser:parse(Tokens),
Exp = {expr,scalar,dyadic,"+",
                       [{'__⍴__',eager,false,float,[2],[1.1,2.2]},
                        {'__⍴__',eager,false,float,[2],[3.3,4.4]}]},
?_assertEqual(Exp, Got).
