-module(parser_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").

%% Tests

basic_dyadic_plus_test_() ->
	Str = "1.1 2.2 + 3.3 4.4",
	Got = pometo:parse_TEST(Str),
	Rho1 = #'$¯¯⍴¯¯'{dimensions = [2], line_no = 1, char_no = 1},
  Arg1 = #ast{op      = Rho1,
              args    = [1.1, 2.2],
              char_no = 1,
              line_no = 1},
	Rho2 = #'$¯¯⍴¯¯'{dimensions = [2], line_no = 1, char_no = 11},
  Arg2 = #ast{op      = Rho2,
              args    = [3.3, 4.4],
              char_no = 11,
              line_no = 1},
	Exp = [#ast{op      = {dyadic, "+"},
	            args    = [Arg1, Arg2],
              char_no = 9,
              line_no = 1}
        ],
	?_assertEqual(Exp, Got).

basic_leading_and_trailing_spaces_test_() ->
  Str = "   1.1 2.2 + 3.3 4.4  \n",
  Got = pometo:parse_TEST(Str),
  Rho1 = #'$¯¯⍴¯¯'{dimensions = [2], line_no = 1, char_no = 4},
  Arg1 = #ast{op      = Rho1,
              args    = [1.1, 2.2],
              char_no = 4,
              line_no = 1},
  Rho2 = #'$¯¯⍴¯¯'{dimensions = [2], line_no = 1, char_no = 14},
  Arg2 = #ast{op      = Rho2,
              args    = [3.3, 4.4],
              char_no = 14,
              line_no = 1},
  Exp = [#ast{op      = {dyadic, "+"},
              args    = [Arg1, Arg2],
              char_no = 12,
              line_no = 1}
         ],
  ?_assertEqual(Exp, Got).

basic_blank_lines_test_() ->
  Str = "1.1 2.2 + 3.3 4.4\n\n3 + 4",
  Got = pometo:parse_TEST(Str),
  Rho1a = #'$¯¯⍴¯¯'{dimensions = [2], line_no = 1, char_no = 1},
  Arg1a = #ast{op      = Rho1a,
               args    = [1.1, 2.2],
               char_no = 1,
               line_no = 1},
  Rho1b = #'$¯¯⍴¯¯'{dimensions = [2], line_no = 1, char_no = 11},
  Arg1b = #ast{op      = Rho1b,
               args    = [3.3, 4.4],
               char_no = 11,
               line_no = 1},
  L1 = #ast{op      = {dyadic, "+"},
            args    = [Arg1a, Arg1b],
            char_no = 9,
            line_no = 1},
  Rho2a = #'$¯¯⍴¯¯'{dimensions = [1], line_no = 3, char_no = 1},
  Arg2a = #ast{op      = Rho2a,
               args    = [3],
               char_no = 1,
               line_no = 3},
  Rho2b = #'$¯¯⍴¯¯'{dimensions = [1], line_no = 3, char_no = 5},
  Arg2b = #ast{op      = Rho2b,
               args    = [4],
               char_no = 5,
               line_no = 3},
  L2 = #ast{op      = {dyadic, "+"},
            args    = [Arg2a, Arg2b],
            char_no = 3,
            line_no = 3},
  Exp = [L1, L2],
  ?_assertEqual(Exp, Got).

basic_monadic_plus_test_() ->
	Str = "+ 3.3 4.4",
	Got = pometo:parse_TEST(Str),
	Rho = #'$¯¯⍴¯¯'{dimensions = [2], line_no = 1, char_no = 3},
  Arg1 = #ast{op      = Rho,
              args    = [3.3, 4.4],
              char_no = 3,
              line_no = 1},
	Exp = [#ast{op      = {monadic, "+"},
	            args    = [Arg1],
              char_no = 1,
              line_no = 1}
        ],
	?_assertEqual(Exp, Got).

basic_two_statement_one_line_test_() ->
  Str = "+ 3.3 4.4 ⋄ - 55 66",
  Got = pometo:parse_TEST(Str),
  RhoA = #'$¯¯⍴¯¯'{dimensions = [2], line_no = 1, char_no = 3},
  RhoB = #'$¯¯⍴¯¯'{dimensions = [2], line_no = 1, char_no = 15},
  ArgA = #ast{op      = RhoA,
              args    = [3.3, 4.4],
              char_no = 3,
              line_no = 1},
  ArgB = #ast{op      = RhoB,
              args    = [55, 66],
              char_no = 15,
              line_no = 1},
  Exp = [#ast{op      = {monadic, "+"},
              args    = [ArgA],
              char_no = 1,
              line_no = 1},
         #ast{op      = {monadic, "-"},
              args    = [ArgB],
              char_no = 13,
              line_no = 1}
        ],
  ?_assertEqual(Exp, Got).

basic_two_statement_two_lines_test_() ->
  Str = "+ 3.3 4.4\n- 5.5 6.6",
  Got = pometo:parse_TEST(Str),
  RhoA = #'$¯¯⍴¯¯'{dimensions = [2], line_no = 1, char_no = 3},
  RhoB = #'$¯¯⍴¯¯'{dimensions = [2], line_no = 2, char_no = 3},
  ArgA = #ast{op      = RhoA,
              args    = [3.3, 4.4],
              char_no = 3,
              line_no = 1},
  ArgB = #ast{op      = RhoB,
              args    = [5.5, 6.6],
              char_no = 3,
              line_no = 2},
  Exp = [#ast{op      = {monadic, "+"},
              args    = [ArgA],
              char_no = 1,
              line_no = 1},
         #ast{op      = {monadic, "-"},
              args    = [ArgB],
              char_no = 1,
              line_no = 2}
          ],
  ?_assertEqual(Exp, Got).
