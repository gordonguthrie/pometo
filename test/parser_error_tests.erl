-module(parser_error_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").

%% Tests
basic_parser_failure_test_() ->
    Str = "MyVar ←← 1 2 3",
    [Err] = pometo:parse_TEST(Str),
    Got = pometo_runtime_format:format_errors([Err#error{expr = Str}]),
    Exp = "\n\nError\n" ++
      "MyVar ←← 1 2 3\n" ++
      "-------^\n" ++
      "SYNTAX ERROR [syntax error before: ← ] on line 1 at character 8",
    % ?debugFmt("~nin basic_parser_failure_test_~nfrom ~p~nGot ~ts~nExp ~ts~n", [Str, Got, Exp]),
    ?_assertEqual(Exp, Got).

two_line_parser_failure_test_() ->
    Str1 = "+ 333",
    Str2 = "MyVar ←← 1 2 3",
    Str = Str1 ++ "\n" ++ Str2,
    %% error dance to get a unicode printable view
    [Got1, Err] = pometo:parse_TEST(Str),
    Got2 = pometo_runtime_format:format_errors([Err#error{expr = Str2}]),
    Rho1 = #'$shape¯'{dimensions = 0,
                      type       = number,
                      line_no    = 1,
                      char_no    = 3},
    L1 = #'$ast¯'{do      = Rho1,
                  args    = 333,
                  line_no = 1,
                  char_no = 3},
    Exp1 = #'$ast¯'{do      = #'$func¯'{do      = ["+"],
                                        type    = monadic,
                                        char_no = 1,
                                        line_no = 1},
                    args    = [L1],
                    line_no = 1,
                    char_no = 1},
    Exp2 = "\n\nError\n" ++
          "MyVar ←← 1 2 3\n" ++
          "-------^\n" ++
          "SYNTAX ERROR [syntax error before: ← ] on line 2 at character 8",
    % ?debugFmt("~nin two_line_parser_failure_test_~nfrom ~p~nGot 1: ~p~n    2: ~ts~nExp 1: ~p~n    2: ~ts~n", [Str, Got1, Got2, Exp1, Exp2]),
    ?_assertEqual([Exp1, Exp2], [Got1, Got2]).

pass_through_lexer_parser_failure_test_() ->
    Str1 = "+ 333",
    Str2 = "¯←😃1 2 3",
    Str = Str1 ++ "\n" ++ Str2,
    [Got1, {error, Got2}] = pometo:parse_TEST(Str),
    Rho1 = #'$shape¯'{dimensions = 0,
                      type       = number,
                      line_no    = 1,
                      char_no    = 3},
    L1 = #'$ast¯'{do      = Rho1,
                  args    = 333,
                  line_no = 1,
                  char_no = 3},
    Exp1 = #'$ast¯'{do      = #'$func¯'{do      = ["+"],
                                        type    = monadic,
                                        char_no = 1,
                                        line_no = 1},
                    args    = [L1],
                    line_no = 1,
                    char_no = 1},
    Exp2 = "\n\nError\n" ++
           "¯←😃1 2 3\n" ++
           "--^\n" ++
           "SYNTAX ERROR [invalid token: 😃 ] on line 2 at character 3",
    % ?debugFmt("~nin pass_through_lexer_parser_failure_test_~nfrom ~p~nGot 1: ~p~n    2: ~ts~nExp 1: ~p~n    2: ~ts~n", [Str, Got1, Got2, Exp1, Exp2]),
    ?_assertEqual({Exp1, Exp2}, {Got1, Got2}).
