-module(stdlib_debug_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

interpreter_debug_usage_test_() ->
  Code = ["⎕debug 1 2 (1 2) 3 4 (5 (6 7)) 8"],
  Got = pometo_test_helper:run_interpreter_test(Code),
  Exp = "In ⎕debug\n" ++
        "*******************************************************************************\n" ++
        "  from line 1 at character no 8\n" ++
        "  Shape: type: mixed (indexed: false) with dimensions [7]\n" ++
        "  arguments: 7\n" ++
        "    1\n" ++
        "    2\n" ++
        "    element is an $ast¯:\n" ++
        "      from line 1 at character no 13\n" ++
        "      Shape: type: number (indexed: false) with dimensions [2]\n" ++
        "      arguments: 2\n" ++
        "        1\n" ++
        "        2\n" ++
        "    3\n" ++
        "    4\n" ++
        "    element is an $ast¯:\n" ++
        "      from line 1 at character no 23\n" ++
        "      Shape: type: mixed (indexed: false) with dimensions [2]\n" ++
        "      arguments: 2\n" ++
        "        5\n" ++
        "        element is an $ast¯:\n" ++
        "          from line 1 at character no 26\n" ++
        "          Shape: type: number (indexed: false) with dimensions [2]\n" ++
        "          arguments: 2\n" ++
        "            6\n" ++
        "            7\n" ++
        "    8\n" ++
        "*******************************************************************************\n" ++
        " on line 1 at character 8\n",
  % ?debugFmt(" in interpreter_debug_usage_test_~nCode:~n~ts~nExp:~n~ts~nGot:~n~ts~n", [Code, Exp, Got]),
  ?_assertEqual(Exp, Got).

compiler_debug_usage_test_() ->
  Code = ["⎕debug 1 2 (1 2) 3 4 (5 (6 7)) 8"],
  Got = pometo_test_helper:run_compiler_test("caveat_lectorreader_beware_1_compiler", Code),
  Exp = "In ⎕debug\n" ++
        "*******************************************************************************\n" ++
        "  from line 1 at character no 8\n" ++
        "  Shape: type: mixed (indexed: false) with dimensions [7]\n" ++
        "  arguments: 7\n" ++
        "    1\n" ++
        "    2\n" ++
        "    element is an $ast¯:\n" ++
        "      from line 1 at character no 13\n" ++
        "      Shape: type: number (indexed: false) with dimensions [2]\n" ++
        "      arguments: 2\n" ++
        "        1\n" ++
        "        2\n" ++
        "    3\n" ++
        "    4\n" ++
        "    element is an $ast¯:\n" ++
        "      from line 1 at character no 23\n" ++
        "      Shape: type: mixed (indexed: false) with dimensions [2]\n" ++
        "      arguments: 2\n" ++
        "        5\n" ++
        "        element is an $ast¯:\n" ++
        "          from line 1 at character no 26\n" ++
        "          Shape: type: number (indexed: false) with dimensions [2]\n" ++
        "          arguments: 2\n" ++
        "            6\n" ++
        "            7\n" ++
        "    8\n" ++
        "*******************************************************************************\n" ++
        " on line 1 at character 8\n",
  % ?debugFmt(" in compiler_debug_usage_test_~nCode:~n~ts~nExp:~n~ts~nGot:~n~ts~n", [Code, Exp, Got]),
  ?_assertEqual(Exp, Got).

compiler_lazy_debug_usage_test_() ->
  Code = ["⎕debug 1 2 (1 2) 3 4 (5 (6 7)) 8"],
  Got = pometo_test_helper:run_compiler_lazy_test("caveat_lectorreader_beware_1_compiler_lazy", Code),
  Exp = "In ⎕debug\n" ++
        "*******************************************************************************\n" ++
        "  from line 1 at character no 8\n" ++
        "  Shape: type: mixed (indexed: false) with dimensions unsized_vector\n" ++
        "  arguments: 7\n" ++
        "    1\n" ++
        "    2\n" ++
        "    element is an $ast¯:\n" ++
        "      from line 1 at character no 13\n" ++
        "      Shape: type: number (indexed: false) with dimensions [2]\n" ++
        "      arguments: 2\n" ++
        "        1\n" ++
        "        2\n" ++
        "    3\n" ++
        "    4\n" ++
        "    element is an $ast¯:\n" ++
        "      from line 1 at character no 23\n" ++
        "      Shape: type: mixed (indexed: false) with dimensions [2]\n" ++
        "      arguments: 2\n" ++
        "        5\n" ++
        "        element is an $ast¯:\n" ++
        "          from line 1 at character no 26\n" ++
        "          Shape: type: number (indexed: false) with dimensions [2]\n" ++
        "          arguments: 2\n" ++
        "            6\n" ++
        "            7\n" ++
        "    8\n" ++
        "*******************************************************************************\n" ++
        " on line 1 at character 8\n",
  % ?debugFmt(" in compiler_lazy_debug_usage_test_~nCode:~n~ts~nExp:~n~ts~nGot:~n~ts~n", [Code, Exp, Got]),
  ?_assertEqual(Exp, Got).

compiler_indexed_debug_usage_test_() ->
  Code = ["⎕debug 1 2 (1 2) 3 4 (5 (6 7)) 8"],
  Got = pometo_test_helper:run_compiler_indexed_test("caveat_lectorreader_beware_1_compiler_indexed", Code),
  Exp = "In ⎕debug\n" ++
        "*******************************************************************************\n" ++
        "  from line 1 at character no 8\n" ++
        "  Shape: type: mixed (indexed: true) with dimensions [7]\n" ++
        "  arguments: 7\n" ++
        "    index   is: 1\n" ++
        "    element is: 1\n" ++
        "    index   is: 2\n" ++
        "    element is: 2\n" ++
        "    index   is: 3\n" ++
        "    element is an $ast¯:\n" ++
        "      from line 1 at character no 13\n" ++
        "      Shape: type: number (indexed: false) with dimensions [2]\n" ++
        "      arguments: 2\n" ++
        "        1\n" ++
        "        2\n" ++
        "    index   is: 4\n" ++
        "    element is: 3\n" ++
        "    index   is: 5\n" ++
        "    element is: 4\n" ++
        "    index   is: 6\n" ++
        "    element is an $ast¯:\n" ++
        "      from line 1 at character no 23\n" ++
        "      Shape: type: mixed (indexed: false) with dimensions [2]\n" ++
        "      arguments: 2\n" ++
        "        5\n" ++
        "        element is an $ast¯:\n" ++
        "          from line 1 at character no 26\n" ++
        "          Shape: type: number (indexed: false) with dimensions [2]\n" ++
        "          arguments: 2\n" ++
        "            6\n" ++
        "            7\n" ++
        "    index   is: 7\n" ++
        "    element is: 8\n" ++
        "*******************************************************************************\n" ++
        " on line 1 at character 8\n",
  % ?debugFmt(" in compiler_indexed_debug_usage_test_~nCode:~n~ts~nExp:~n~ts~nGot:~n~ts~n", [Code, Exp, Got]),
  ?_assertEqual(Exp, Got).

compiler_force_index_debug_usage_test_() ->
  Code = ["⎕debug 1 2 (1 2) 3 4 (5 (6 7)) 8"],
  Got = pometo_test_helper:run_compiler_force_index_test("caveat_lectorreader_beware_1_compiler_force_index", Code),
  Exp = "In ⎕debug\n" ++
        "*******************************************************************************\n" ++
        "  from line 1 at character no 8\n" ++
        "  Shape: type: mixed (indexed: false) with dimensions [7]\n" ++
        "  arguments: 7\n" ++
        "    1\n" ++
        "    2\n" ++
        "    element is an $ast¯:\n" ++
        "      from line 1 at character no 13\n" ++
        "      Shape: type: number (indexed: false) with dimensions [2]\n" ++
        "      arguments: 2\n" ++
        "        1\n" ++
        "        2\n" ++
        "    3\n" ++
        "    4\n" ++
        "    element is an $ast¯:\n" ++
        "      from line 1 at character no 23\n" ++
        "      Shape: type: mixed (indexed: false) with dimensions [2]\n" ++
        "      arguments: 2\n" ++
        "        5\n" ++
        "        element is an $ast¯:\n" ++
        "          from line 1 at character no 26\n" ++
        "          Shape: type: number (indexed: false) with dimensions [2]\n" ++
        "          arguments: 2\n" ++
        "            6\n" ++
        "            7\n" ++
        "    8\n" ++
        "*******************************************************************************\n" ++
        " on line 1 at character 8\n",
  % ?debugFmt(" in compiler_force_index_debug_usage_test_~nCode:~n~ts~nExp:~n~ts~nGot:~n~ts~n", [Code, Exp, Got]),
  ?_assertEqual(Exp, Got).

compiler_force_unindex_debug_usage_test_() ->
  Code = ["⎕debug 1 2 (1 2) 3 4 (5 (6 7)) 8"],
  Got = pometo_test_helper:run_compiler_force_unindex_test("caveat_lectorreader_beware_1_compiler_force_unindex", Code),
  Exp = "In ⎕debug\n" ++
        "*******************************************************************************\n" ++
        "  from line 1 at character no 8\n" ++
        "  Shape: type: mixed (indexed: true) with dimensions [7]\n" ++
        "  arguments: 7\n" ++
        "    index   is: 1\n" ++
        "    element is: 1\n" ++
        "    index   is: 2\n" ++
        "    element is: 2\n" ++
        "    index   is: 3\n" ++
        "    element is an $ast¯:\n" ++
        "      from line 1 at character no 13\n" ++
        "      Shape: type: number (indexed: false) with dimensions [2]\n" ++
        "      arguments: 2\n" ++
        "        1\n" ++
        "        2\n" ++
        "    index   is: 4\n" ++
        "    element is: 3\n" ++
        "    index   is: 5\n" ++
        "    element is: 4\n" ++
        "    index   is: 6\n" ++
        "    element is an $ast¯:\n" ++
        "      from line 1 at character no 23\n" ++
        "      Shape: type: mixed (indexed: false) with dimensions [2]\n" ++
        "      arguments: 2\n" ++
        "        5\n" ++
        "        element is an $ast¯:\n" ++
        "          from line 1 at character no 26\n" ++
        "          Shape: type: number (indexed: false) with dimensions [2]\n" ++
        "          arguments: 2\n" ++
        "            6\n" ++
        "            7\n" ++
        "    index   is: 7\n" ++
        "    element is: 8\n" ++
        "*******************************************************************************\n" ++
        " on line 1 at character 8\n",
  % ?debugFmt(" in compiler_force_unindex_debug_usage_test_~nCode:~n~ts~nExp:~n~ts~nGot:~n~ts~n", [Code, Exp, Got]),
  ?_assertEqual(Exp, Got).

