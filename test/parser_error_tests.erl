-module(parser_error_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").

%% Tests
basic_parser_failure_test_() ->
    Str = "MyVar ←← 1 2 3",
    [{error, Got}] = pometo:parse_TEST(Str),
    Exp = "Error\n" ++
		  "MyVar ←← 1 2 3\n" ++
		  "-------^\n" ++
          "SYNTAX ERROR (syntax error before: :{let_op,8,[8592],[8592]}) on line 1 at character 8\n\n",
    % ?debugFmt("~nin basic_parser_failure_test_~nfrom ~p~nGot ~ts~nExp ~ts~n", [Str, Got, Exp]),
    ?_assertEqual(Exp, Got).

two_line_parser_failure_test_() ->
    Str = "+ 333\nMyVar ←← 1 2 3",
    %% error dance to get a unicode printable view
    [Got1, {error, Got2}] = pometo:parse_TEST(Str),
    Rho1 = #'$shape¯'{dimensions = 0,
                      type       = number,
                      line_no    = 1,
                      char_no    = 3},
    L1 = #'$ast¯'{op      = Rho1,
                  args    = 333,
                  line_no = 1,
                  char_no = 3},
    Exp1 = #'$ast¯'{op      = {monadic, "+"},
                    args    = [L1],
                    line_no = 1,
                    char_no = 1},
    Exp2 = "Error\n" ++
          "MyVar ←← 1 2 3\n" ++
          "-------^\n" ++
          "SYNTAX ERROR (syntax error before: :{let_op,8,[8592],[8592]}) on line 2 at character 8\n\n",
    % ?debugFmt("~nin two_line_parser_failure_test_~nfrom ~p~nGot 1: ~p~n    2: ~ts~nExp 1: ~p~n    2: ~ts~n", [Str, Got1, Got2, Exp1, Exp2]),
    ?_assertEqual([Exp1, Exp2], [Got1, Got2]).

pass_through_lexer_parser_failure_test_() ->
    Str = "+ 333\n¯←😃1 2 3",
    [Got1, {error, Got2}] = pometo:parse_TEST(Str),
    Rho1 = #'$shape¯'{dimensions = 0,
                      type       = number,
                      line_no    = 1,
                      char_no    = 3},
    L1 = #'$ast¯'{op      = Rho1,
                  args    = 333,
                  line_no = 1,
                  char_no = 3},
    Exp1 = #'$ast¯'{op      = {monadic, "+"},
                    args    = [L1],
                    line_no = 1,
                    char_no = 1},
    Exp2 = "Error\n" ++
           "¯←😃1 2 3\n" ++
           "--^\n" ++
           "SYNTAX ERROR (invalid token:😃) on line 2 at character 3\n" ++
           "\n",
    % ?debugFmt("~nin pass_through_lexer_parser_failure_test_~nfrom ~p~nGot 1: ~p~n    2: ~ts~nExp 1: ~p~n    2: ~ts~n", [Str, Got1, Got2, Exp1, Exp2]),
    ?_assertEqual({Exp1, Exp2}, {Got1, Got2}).
