-module(compiler_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests

basic_compiler_test_() ->
	Str = "A ← 4 5 6 ⋄ B ← 6 7 ¯8 ⍝ including comments\n" ++
	      "A + B",
	Got = pometo:compile_load_and_run_TEST(Str, "basic_compiler_test_module"),
	Exp = "10 12 ¯2",
    % ?debugFmt("in basic_compiler_test_~nGot ~ts~nExp ~ts~n", [Got, Exp]),
	?_assertEqual(Exp, Got).
