-module(interpreter_error_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests

basic_interpreter_failure_test_() ->
	Str = "A ← 4 5 6 ⋄ B ← 6 7 ¯8 ⍝ including comments\n" ++
	      "A + B\n" ++
	      "MyVar ←← 1 2 3 ⋄ 55 66 + 77 88\n" ++
	      "Myvr ←×← 1 2 3",
	Got = pometo:interpret_TEST(Str),
    Exp = "Error\n" ++
		  "MyVar ←← 1 2 3 ⋄ 55 66 + 77 88\n" ++
		  "-------^\n" ++
          "SYNTAX ERROR (syntax error before: :{let_op,8,[8592],[8592]}) on line 3 at character 8" ++
          "\n\nError\n" ++
		  "Myvr ←×← 1 2 3\n" ++
		  "------^\n" ++
          "SYNTAX ERROR (syntax error before: :{scalar_fn,7,\"×\",\"×\"}) on line 4 at character 7",
    % ?debugFmt("in basic_interpreter_failure_test_~nGot ~ts~nExp ~ts~n", [Got, Exp]),
	?_assertEqual(Exp, Got).

variable_redefinition_same_line_test_() ->
	Str = "A ← 4 5 6 ⋄ A ← 6 7 ¯8 ⍝ including comments\n",
	Got = pometo:interpret_TEST(Str),
    Exp = "Error\n" ++
		  "A ← 4 5 6 ⋄ A ← 6 7 ¯8 ⍝ including comments\n" ++
		  "------------^\n" ++
		  "VARIABLE REASSIGNED (A:was previously assigned on line 1 at char 1) on line 1 at character 13",
    % ?debugFmt("in variable_redefinition_same_line_test_~nGot ~ts~nExp ~ts~n", [Got, Exp]),
	?_assertEqual(Exp, Got).

variable_triple_redefinition_same_line_test_() ->
	Str = "A ← 4 5 6 ⋄ A ← 6 7 8 ⋄ A ← 1 2 3 ⋄ A ← 9 8 7",
	Got = pometo:interpret_TEST(Str),
    Exp = "Error\n" ++
		  "A ← 4 5 6 ⋄ A ← 6 7 8 ⋄ A ← 1 2 3 ⋄ A ← 9 8 7\n" ++
		  "------------^\n" ++
		  "VARIABLE REASSIGNED (A:was previously assigned on line 1 at char 1) on line 1 at character 13" ++
		  "\n\nError\n" ++
		  "A ← 4 5 6 ⋄ A ← 6 7 8 ⋄ A ← 1 2 3 ⋄ A ← 9 8 7\n" ++
		  "------------------------^\n" ++
		  "VARIABLE REASSIGNED (A:was previously assigned on line 1 at char 13) on line 1 at character 25" ++
		  "\n\nError\n" ++
		  "A ← 4 5 6 ⋄ A ← 6 7 8 ⋄ A ← 1 2 3 ⋄ A ← 9 8 7\n" ++
		  "------------------------------------^\n" ++
		  "VARIABLE REASSIGNED (A:was previously assigned on line 1 at char 25) on line 1 at character 37",
    % ?debugFmt("in variable_triple_redefinition_same_line_test_~nGot ~ts~nExp ~ts~n", [Got, Exp]),
	?_assertEqual(Exp, Got).

variable_redefinition_test_() ->
	Str = "A ← 4 5 6\n\nA ← 6 7 8",
	Got = pometo:interpret_TEST(Str),
    Exp = "Error\n" ++
		  "A ← 6 7 8\n" ++
		  "^\n" ++
		  "VARIABLE REASSIGNED (A:was previously assigned on line 1 at char 1) on line 3 at character 1",
    % ?debugFmt("in variable_redefinition_test_~nGot ~ts~nExp ~ts~n", [Got, Exp]),
	?_assertEqual(Exp, Got).

%% move to errors
undefined_variable_failure_test_() ->
	Str = "A ← 4 5 6 ⋄ B ← 6 7 ¯8 ⍝ including comments\n" ++
	      "A + C",
	Got = pometo:interpret_TEST(Str),
	Exp = "Error\n" ++
		  "A + C\n" ++
		  "----^\n" ++
		  "VARIABLE NOT DEFINED (C:variable is not defined) on line 2 at character 5",
    % ?debugFmt("in undefined_variable_failure_test_Got ~ts~nExp ~ts~n", [Got, Exp]),
	?_assertEqual(Exp, Got).