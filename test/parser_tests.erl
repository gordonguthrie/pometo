-module(parser_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests

basic_dyadic_plus_test_() ->
	Str = "1.1 2.2 + 3.3 4.4",
	{ok, Tokens} = pometo_lexer:get_tokens(Str),
	{ok, Got} = pometo_parser:parse(Tokens),
	Exp = {{liffey, {dyadic, "+"},
	                [
                     {liffey, {'¯¯⍴¯¯', eager, false, [2]}, [1.1, 2.2]},
                     {liffey, {'¯¯⍴¯¯', eager, false, [2]}, [3.3, 4.4]}
                    ]}, #{}},
	?_assertEqual(Exp, Got).

basic_monadic_plus_test_() ->
	Str = "+ 3.3 4.4",
	{ok, Tokens} = pometo_lexer:get_tokens(Str),
	{ok, Got} = pometo_parser:parse(Tokens),
	Exp = {{liffey, {monadic, "+"},
	                [
                     {liffey, {'¯¯⍴¯¯', eager, false, [2]}, [3.3, 4.4]}
                    ]}, #{}},
	?_assertEqual(Exp, Got).
