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
          "SYNTAX ERROR (syntax error before: :{let_op,8,[8592],[8592]}) on line 3 at character 8\n\n" ++
          "Error\n" ++
		  "Myvr ←×← 1 2 3\n" ++
		  "------^\n" ++
          "SYNTAX ERROR (syntax error before: :{scalar_fn,7,\"×\",\"×\"}) on line 4 at character 7\n\n",
    % ?debugFmt("~nGot ~ts~nExp ~ts~n", [Got, Exp]),
	?_assertEqual(Exp, Got).
