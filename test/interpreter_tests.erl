-module(interpreter_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests

basic_interpreter_test_() ->
	Str = "A ← 4 5 6 ⋄ B ← 6 7 ¯8 ⍝ including comments\n" ++
	      "A + B",
	Got = pometo:interpret_TEST(Str),
	Exp = "4 5 6\n" ++
	      "6 7 ¯8\n" ++
	      "10 12 ¯2",
    % ?debugFmt("~nGot ~ts~nExp ~ts~n", [Got, Exp]),
	?_assertEqual(Exp, Got).


