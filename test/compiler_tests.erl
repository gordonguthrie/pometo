-module(compiler_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests

basic_compiler_test_xx() ->
	Str = "A ← 4 5 6 ⋄ B ← 6 7 ¯8 ⍝ including comments\n" ++
	      "A + B",
	Got = pometo:compile_load_and_run_TEST(Str),
	Exp = "10 11 ¯2",
	?_assertEqual(Exp, Got).
