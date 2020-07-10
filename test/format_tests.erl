-module(format_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("parser_records.hrl").
-include("runtime_include.hrl").

%%%
%%% Test the fragment builder
%%%

basic_fragments_scalar_test_() ->
    Shape = pometo:run_for_format_TEST("1.1", "basic_fragments_scalar_test_"),
    Got = pometo_runtime_format:build_segments_TEST(Shape),
    Exp = [line([
                    leaf(["1.1"], 3, 1)
                ])
        ],
    % ?debugFmt("in basic_fragments_vector_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_fragments_scalar_edge_case_test_() ->
    %% a single scalar cant be turned into a vector by brackets
    Shape = pometo:run_for_format_TEST("1 (2) 3", "basic_fragments_scalar_edge_case_test_"),
    Got = pometo_runtime_format:build_segments_TEST(Shape),
    Exp = [line([
                    leaf(["1"], 1, 1),
                    leaf(["2"], 1, 1),
                    leaf(["3"], 1, 1)
                ])
        ],
    % ?debugFmt("in basic_fragments_scalar_edge_case_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_fragments_vector_test_() ->
    Shape = pometo:run_for_format_TEST("1.1 2.2", "basic_fragments_vector_test_"),
    Got = pometo_runtime_format:build_segments_TEST(Shape),
    Exp = [line([
                    leaf(["1.1"], 3, 1),
                    leaf(["2.2"], 3, 1)
                ])
          ],
    % ?debugFmt("in basic_fragments_vector_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_fragments_nested_vector_test_() ->
    Shape = pometo:run_for_format_TEST("(1 2)(3 4)", "basic_fragments_nested_vector_test_"),
    % ?debugFmt("in basic_fragments_nested_vector_test_ Shape is ~p~n", [Shape]),
    Got = pometo_runtime_format:build_segments_TEST(Shape),
    Exp = [line([
                node([
                      leaf(["1"], 1, 1),
                      leaf(["2"], 1, 1)
                      ], 5, 3, boxed),
                node([
                      leaf(["3"], 1, 1),
                      leaf(["4"], 1, 1)
                      ], 5, 3, boxed)
                ])
        ],

    % ?debugFmt("in basic_fragments_nested_vector_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_rhoed_fragments_vector_test_() ->
    Shape = pometo:run_for_format_TEST("2 2 ⍴ 1 2 3 4", "basic_rhoed_fragments_nested_vector_test_"),
    % ?debugFmt("in basic_rhoed_fragments_vector_test_ Shape is ~p~n", [Shape]),
    Got = pometo_runtime_format:build_segments_TEST(Shape),
    Exp = [
           line([
                    leaf(["1"], 1, 1),
                    leaf(["2"], 1, 1)
                ]),
           line([
                    leaf(["3"], 1, 1),
                    leaf(["4"], 1, 1)
                ])
          ],
    % ?debugFmt("in basic_rhoed_fragments_vector_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_rhoed_fragments_nested_vector_test_() ->
    Shape = pometo:run_for_format_TEST("2 2 ⍴ 1 (22 22) 3 (4444 4444 4444 4444)", "basic_rhoed_fragments_nested_vector_test_"),
    % should look like this
    % ┌─┬┬───────────────────┐
    % │1││22 22              │
    % ├─┼┼───────────────────┤
    % │3││4444 4444 4444 4444│
    % └─┴┴───────────────────┘
    % 123456789012345678901234

    % ?debugFmt("in basic_rhoed_fragments_nested_vector_test_ Shape is ~p~n", [Shape]),
    Got = pometo_runtime_format:build_segments_TEST(Shape),
    Exp = [
            line([
                    leaf(["1"], 1, 3),
                    node([
                            leaf(["22"], 2, 1),
                            leaf(["22"], 2, 1)
                          ], 7, 3, boxed)
                ]),
            line([
                    leaf(["3"], 1, 3),
                    node([
                            leaf(["4444"], 4, 1),
                            leaf(["4444"], 4, 1),
                            leaf(["4444"], 4, 1),
                            leaf(["4444"], 4, 1)
                          ], 21, 3, boxed)
                ])
          ],
    % ?debugFmt("in basic_rhoed_fragments_nested_vector_test_~nGot ~p~nExp ~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

%%%
%%% Overall tests
%%%

basic_one_line_format_test_() ->
    Str = "_MyVariable ← 1 2 ¯3 4.0j0.0 5.0",
         %0123456789012345678901234567890123456789
    Got = pometo:interpret_TEST(Str),
    Exp = "1 2 ¯3 4.0J0.0 5.0",
    % ?debugFmt("in basic_one_line_format_test_~nGot~n~ts~nExp~n~ts~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_nested_format_test_() ->
    Str = "MyVariable ← 1 2 3 4 5 (1 2 3 4 5)",
    Got = pometo:interpret_TEST(Str),
    Exp = "          ┌─────────┐\n" ++
          "1 2 3 4 5 │1 2 3 4 5│\n" ++
          "          └─────────┘",
    % ?debugFmt("in basic_nested_format_test_~nGot~n~ts~n--~nExp~n~ts~n--", [Got, Exp]),
    ?_assertEqual(Exp, Got).

double_nested_format_test_() ->
    Str = "MyVariable ← (1 2 3 4 5) (1 2 3 4 5)",
    Got = pometo:interpret_TEST(Str),
    Exp = "┌─────────┐ ┌─────────┐\n" ++
          "│1 2 3 4 5│ │1 2 3 4 5│\n" ++
          "└─────────┘ └─────────┘",
    % ?debugFmt("in double_nested_format_test_~nGot~n~ts~n--Exp~n~ts~n--", [Got, Exp]),
    ?_assertEqual(Exp, Got).


basic_one_line_bust_measure_format_test_() ->
    Str = "MyVariable ← 1 2 3 4 5 6 7 8 9 0" ++
    	              " 1 2 3 4 5 6 7 8 9 0"   ++
    	              " 1 2 3 4 5 6 7 8 9 0"   ++
    	              " 1 2 3 4 5 6 7 8 9 0\n" ++
    	   "MyVariable + 1",
    Got = pometo:interpret_TEST(Str),
    Exp = "2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 6 7 8 9 10 [20 chars deleted ]",
    % ?debugFmt("in basic_one_line_bust_measure_format_test_~nGot~n~ts~nExp~n~ts~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_nested_bust_measure_format_test_() ->
    Str = "MyVariable ← (1 2 3 4 5 6 7 8 9 0" ++
    	              " 1 2 3 4 5 6 7 8 9 0"    ++
    	              " 1 2 3 4 5 6 7 8 9 0"    ++
    	              " 1 2 3 4 5 6 7 8 9 0)"   ++
                    "(1 2 3 4 5 6 7 8 9 0"    ++
                    " 1 2 3 4 5 6 7 8 9 0"    ++
                    " 1 2 3 4 5 6 7 8 9 0"    ++
                    " 1 2 3 4 5 6 7 8 9 0)"   ++
                    "\n"                      ++
                    "MyVariable + 0",
    Got = pometo:interpret_TEST(Str),
    Exp = "┌───────────────────────────────────────────────────────────[100 chars deleted ]\n" ++
          "│1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0[100 chars deleted ]\n" ++
          "└───────────────────────────────────────────────────────────[100 chars deleted ]",
    % ?debugFmt("in basic_nested_bust_measure_format_test_~nGot~n~p~nExp~n~p~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

basic_height_measure_busted_test_() ->
  Str = "4 4 4 4 ⍴ 1 2 3 4 5 6 7 8 9 0",
    Got = pometo:interpret_TEST(Str),
    Exp = "1 2 3 4\n" ++
          "5 6 7 8\n" ++
          "9 0 1 2\n" ++
          "3 4 5 6\n" ++
          "\n"        ++
          "7 8 9 0\n" ++
          "1 2 3 4\n" ++
          "5 6 7 8\n" ++
          "9 0 1 2\n" ++
          "\n"        ++
          "3 4 5 6\n" ++
          "7 8 9 0\n" ++
          "1 2 3 4\n" ++
          "5 6 7 8\n" ++
          "\n"        ++
          "9 0 1 2\n" ++
          "3 4 5 6\n" ++
          "7 8 9 0\n" ++
          "1 2 3 4\n" ++
          "\n"        ++
          "5 6 7 8\n" ++
          "9 0 1 2\n" ++
          "3 4 5 6\n" ++
          "7 8 9 0\n" ++
          "\n"        ++
          "1 2 3 4\n" ++
          "5 6 7 8\n" ++
          "9 0 1 2\n" ++
          "3 4 5 6\n" ++
          "\n"        ++
          "7 8 9 0\n" ++
          "1 2 3 4\n" ++
          "5 6 7 8\n" ++
          "9 0 1 2\n" ++
          "\n"        ++
          "3 4 5 6\n" ++
          "7 8 9 0\n" ++
          "1 2 3 4\n" ++
          "5 6 7 8\n" ++
          "\n"        ++
          "9 0 1 2\n" ++
          "3 4 5 6\n" ++
          "7 8 9 0\n" ++
          "1 2 3 4\n" ++
          "\n"        ++
          "5 6 7 8\n" ++
          "9 0 1 2\n" ++
          "3 4 5 6\n" ++
          "7 8 9 0\n" ++
          "\n"        ++
          "1 2 3 4\n" ++
          "5 6 7 8\n" ++
          "9 0 1 2\n" ++
          "3 4 5 6\n" ++
          "\n"        ++
          "7 8 9 0\n" ++
          "1 2 3 4\n" ++
          "5 6 7 8\n" ++
          "9 0 1 2\n" ++
          "\n"        ++
          "3 4 5 6\n" ++
          "7 8 9 0\n" ++
          "1 2 3 4\n" ++
          "5 6 7 8\n" ++
          "\n"        ++
          "9 0 1 2\n" ++
          "3 4 5 6\n" ++
          "7 8 9 0\n" ++
          "1 2 3 4\n" ++
          "\n"        ++
          "5 6 7 8\n" ++
          "[... 31 lines cut...]",
    % ?debugFmt("in basic_height_measure_busted_test_~nGot~n~ts~nExp~n~ts~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

kitchen_sink_test_() ->
    Str = "2 2 ⍴ (1 22) (1 22 (333 4444)) 55555",
    Got  = pometo:interpret_TEST(Str),
    Exp  = "┌─────┐ ┌───────────────┐\n" ++
           "│ 1 22│ │1 22 ┌────────┐│\n" ++
           "└─────┘ │     │333 4444││\n" ++
           "        │     └────────┘│\n" ++
           "        └───────────────┘\n" ++
           "        ┌───────────────┐\n" ++
           "  55555 │           1 22│\n" ++
           "        └───────────────┘",
    % ?debugFmt("in kitchen_sink_test_~nGot~n~ts~n--~nExp~n~ts~n--~n", [Got, Exp]),
    ?_assertEqual(Exp, Got).

%%%
%%% Helper Fns
%%%

node(Strings, W, H, Boxed) when is_list(Strings) andalso
                                is_integer(W)    andalso
                                is_integer(H)    andalso
                                (Boxed == none   orelse
                                 Boxed == boxed  orelse
                                 Boxed == blankboxed) ->
    #fmt_segment{strings = Strings,
                 width   = W,
                 height  = H,
                 is_leaf = false,
                 boxing  = Boxed}.

leaf(Strings, W, H) when is_list(Strings) andalso
                         is_integer(W)    andalso
                         is_integer(H)    ->
    #fmt_segment{strings = Strings,
                 width   = W,
                 height  = H,
                 is_leaf = true}.

line(List) when is_list(List) -> #fmt_line{segs = List}.