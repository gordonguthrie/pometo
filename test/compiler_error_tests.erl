-module(compiler_error_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

%% Tests

basic_compiler_failure_test_() ->
	Str = "A ← 4 5 6",
	Got = pometo:compile_load_and_run_TEST(Str, "basic_compiler_error_test_module"),
	Exp = "\n\nError\n"        ++
		  "dunno? FIXME\n" ++
		  "^\n"            ++
		  "UNUSED VARIABLE (variable is unused:A) on line 1 at character 1",
    % ?debugFmt("in basic_compiler_failure_test_~nfrom ~p~nGot ~ts~nExp ~ts~n", [Str, Got, Exp]),
	?_assertEqual(Exp, Got).
