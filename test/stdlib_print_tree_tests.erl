-module(stdlib_print_tree_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("tree_printing_records.hrl").
-include("runtime_include.hrl").
-include("comments.hrl").

%%%
%%% Test tree printer
%%%

%%% there are four sets of tests:
%%% * the first tests the conversion of the AST into the elements:
%%%   * column and row position
%%%   * length of the string
%%% * the second set works over the elements and calculates the maximum
%%%   width of a given row across all columns
%%% * the third set applies those sizes to the cells to offset them the
%%%   appropriate amount
%%% * the fourth set tests the actual printing of the tree

%%%
%%% Test the preparation of the print cells
%%%

simplest_tree_print_cells_test_() ->
  Code = "1 2",
  Got = make_cells(Code),
  Exp = [
         #printcell{row = 1, col = 1, width = 17, text = "shape: [2]/number", needs_roof = false},
         #printcell{row = 2, col = 1, width = 1,  text = "1",                 needs_roof = initial},
         #printcell{row = 2, col = 2, width = 1,  text = "2",                 needs_roof = last}
         ],
  % ?debugFmt("in simplest_tree_print_cells_test_~nExp: ~p~nGot: ~p~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

simple_tree_print_cells_test_() ->
  Code = "A ← 1 22",
  Got = make_cells(Code),
  Exp = [
         #printcell{row = 1, col = 1, width = 6,  text = "let_op",            needs_roof = false},
         #printcell{row = 2, col = 1, width = 5,  text = "'A_0'",             needs_roof = initial},
         #printcell{row = 2, col = 2, width = 17, text = "shape: [2]/number", needs_roof = last},
         #printcell{row = 3, col = 2, width = 1,  text = "1",                 needs_roof = initial},
         #printcell{row = 3, col = 3, width = 2,  text = "22",               needs_roof = last}
         ],
  % ?debugFmt("in simple_tree_print_cells_test_~nExp: ~p~nGot: ~p~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

simplest_nested_tree_test_() ->
  Code = "1 (2 3)",
  Got = make_cells(Code),
  Exp = [
          {printcell,1,1,16,0,0,false,   "shape: [2]/mixed"},
          {printcell,2,1,1, 0,0,initial, "1"},
          {printcell,2,2,17,0,0,last,    "shape: [2]/number"},
          {printcell,3,2,1, 0,0,initial, "2"},
          {printcell,3,3,1, 0,0,last,    "3"}
        ],
  % ?debugFmt("in simplest_nested_tree_test_~nExp: ~p~nGot: ~p~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

simple_nested_tree_test_() ->
  Code = "1 (2 3) 4",
  Got = make_cells(Code),
  Exp = [
          {printcell,1,1,16,0,0,false,     "shape: [3]/mixed"},
          {printcell,2,1,1, 0,0,initial,   "1"},
          {printcell,2,2,17,0,0,subsequent,"shape: [2]/number"},
          {printcell,2,4,1, 0,0,last,      "4"},
          {printcell,3,2,1, 0,0,initial,   "2"},
          {printcell,3,3,1, 0,0,last,      "3"}
        ],
  % ?debugFmt("in simple_nested_tree_test_~nExp: ~p~nGot: ~p~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

nested_tree_test_() ->
  Code = "1 2 (1 2) 3 4 (5 (6 7)) 8",
  Got = make_cells(Code),
  Exp = [
          {printcell,1,1,16, 0,0,false,     "shape: [7]/mixed"},
          {printcell,2,1,1,  0,0,initial,   "1"},
          {printcell,2,2,1,  0,0,subsequent,"2"},
          {printcell,2,3,17, 0,0,subsequent,"shape: [2]/number"},
          {printcell,2,5,1,  0,0,subsequent,"3"},
          {printcell,2,6,1,  0,0,subsequent,"4"},
          {printcell,2,7,16, 0,0,subsequent,"shape: [2]/mixed"},
          {printcell,2,10,1, 0,0,last,      "8"},
          {printcell,3,3,1,  0,0,initial,   "1"},
          {printcell,3,4,1,  0,0,last,      "2"},
          {printcell,3,7,1,  0,0,initial,   "5"},
          {printcell,3,8,17, 0,0,last,      "shape: [2]/number"},
          {printcell,4,8,1,  0,0,initial,   "6"},
          {printcell,4,9,1,  0,0,last,      "7"}
        ],
  % ?debugFmt("in nested_tree_test_~nExp: ~p~nGot: ~p~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

%%%
%%% Test the preparation of the sizes
%%%

simplest_tree_sizing_test_() ->
  Code = "1 2",
  Got = make_sizes(Code),
  Exp = {#{1 => 19, 2 => 3}, 2},
  ?_assertEqual(Exp, Got).

simple_tree_sizing_test_() ->
  Code = "A ← 1 22",
  Got = make_sizes(Code),
  Exp = {#{1 => 8, 2 => 19, 3 => 4}, 3},
  ?_assertEqual(Exp, Got).

%%%
%%% Test adding offsets to the sizes
%%%

%% This test tests the parsing of the structure
%% for the printed tree for this expression
%% 1 2
%%
%% The final output should look like this:
%%
%%     1234567890123
%% -----------------
%% 1 | shape: [2]..  
%% 1 | |
%% 3 | -------------
%% 4 | |           |
%% 5 | 1           2
simplest_tree_offsets_test_() ->
  Code = "1 22",
  Got = make_offsets(Code),
  Exp = [
         #printcell{row = 1, col = 1, width = 17, x_offset = 1,  y_offset = 4, needs_roof = false,      text = "shape: [2]/number"},
         #printcell{row = 2, col = 1, width = 1,  x_offset = 1,  y_offset = 8, needs_roof = initial,    text = "1"},
         #printcell{row = 2, col = 1, width = 1,  x_offset = 1,  y_offset = 5, needs_roof = false,      text = "|"},
         #printcell{row = 2, col = 1, width = 19, x_offset = 1,  y_offset = 6, needs_roof = false,      text = "├------------------"},
         #printcell{row = 2, col = 1, width = 1,  x_offset = 1,  y_offset = 7, needs_roof = false,      text = "|"},
         #printcell{row = 2, col = 2, width = 2,  x_offset = 20, y_offset = 8, needs_roof = last,       text = "22"},
         #printcell{row = 2, col = 2, width = 1,  x_offset = 20, y_offset = 6, needs_roof = false,      text = "┐"},
         #printcell{row = 2, col = 2, width = 1,  x_offset = 20, y_offset = 7, needs_roof = false,      text = "|"}
         ],
  % ?debugFmt("in simplest_tree_offsets_test_~nGot is~n~p~nExpt is~n~p~n", [Got, Exp]),
  ?_assertEqual(Exp, Got).

%% This test tests the parsing of the structure
%% for the printed tree for this expression
%% A ← 1 2
%%
%% The final output should look like this:
%%
%%     123456789012345678901
%% -------------------------
%% 1 | let_op
%% 2 | |
%% 3 | ---------
%% 4 | |       |
%% 5 | 'A_0'.. shape: [2]..
%% 6 |         |
%% 7 |         -------------
%% 8 |         |           |
%% 9 |         1           22
simple_tree_offsets_test_() ->
  Code = "A ← 1 22",
  Got = make_offsets(Code),
  Exp = [
         {printcell,1,1,6,1,4,false,"let_op"},
         {printcell,2,1,5,1,8,initial,"'A_0'"},
         {printcell,2,1,1,1,5,false,"|"},
         {printcell,2,1,8,1,6,false,"├-------"},
         {printcell,2,1,1,1,7,false,"|"},
         {printcell,2,2,17,9,8,last,"shape: [2]/number"},
         {printcell,2,2,1,9,6,false,"┐"},
         {printcell,2,2,1,9,7,false,"|"},
         {printcell,3,2,1,9,12,initial,"1"},
         {printcell,3,2,1,9,9,false,"|"},
         {printcell,3,2,19,9,10,false,"├------------------"},
         {printcell,3,2,1,9,11,false,"|"},
         {printcell,3,3,2,28,12,last,"22"},
         {printcell,3,3,1,28,10,false,"┐"},
         {printcell,3,3,1,28,11,false,"|"}
         ],
  % ?debugFmt("in simple_tree_offsets_test_~nGot is~n~p~nExpt is~n~p~n", [Got, Exp]),
  ?_assertEqual(Exp, Got).

%%%
%%% Print Cells
%%%

simplest_tree_print_test_() ->
  Code = "1 22",
  #comment{msg = Got} = print(Code),
  Exp = "shape: [2]/number      \n" ++
        "|                      \n" ++
        "├------------------┐   \n" ++
        "|                  |   \n" ++
        "1                  22  \n",
  % ?debugFmt("in simplest_tree_print_test_~nExp:~n~ts~nGot:~n~ts~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

simple_tree_print_test_() ->
  Code = "A ← 1 22",
  #comment{msg = Got} = print(Code),
  Exp = "let_op                         \n" ++
        "|                              \n" ++
        "├-------┐                      \n" ++
        "|       |                      \n" ++
        "'A_0'   shape: [2]/number      \n" ++
        "        |                      \n" ++
        "        ├------------------┐   \n" ++
        "        |                  |   \n" ++
        "        1                  22  \n",
  % ?debugFmt("in simple_tree_print_test_~nExp:~n~ts~nGot:~n~ts~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

simplest_nested_tree_print_test_() ->
  Code = "1 (2 3)",
  #comment{msg = Got} = print(Code),
  %     "123456789012345678901234567890"
  Exp = "shape: [2]/mixed                        \n" ++
        "|                                       \n" ++
        "├-----------------┐                     \n" ++
        "|                 |                     \n" ++
        "1                 shape: [2]/number     \n" ++
        "                  |                     \n" ++
        "                  ├------------------┐  \n" ++
        "                  |                  |  \n" ++
        "                  2                  3  \n",
  % ?debugFmt("in simplest_nested_tree_print_test_~nExp:~n~ts~nGot:~n~ts~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

simple_nested_tree_print_test_() ->
  Code = "1 (2 3) 4",
  #comment{msg = Got} = print(Code),
  %     "123456789012345678901234567890"
  Exp = "shape: [3]/mixed                           \n" ++
        "|                                          \n" ++
        "├-----------------┬---------------------┐  \n" ++
        "|                 |                     |  \n" ++
        "1                 shape: [2]/number     4  \n" ++
        "                  |                        \n" ++
        "                  ├------------------┐     \n" ++
        "                  |                  |     \n" ++
        "                  2                  3     \n",
  % ?debugFmt("in simple_nested_tree_print_test_~nExp:~n~ts~nGot:~n~ts~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

nested_tree_print_test_() ->
  Code = "1 2 (1 2) 3 4 (5 (6 7)) 8",
  #comment{msg = Got} = print(Code),
  Exp = "shape: [7]/mixed                                                                            \n" ++
        "|                                                                                           \n" ++
        "├-----------------┬--┬---------------------┬--┬--┬---------------------------------------┐  \n" ++
        "|                 |  |                     |  |  |                                       |  \n" ++
        "1                 2  shape: [2]/number     3  4  shape: [2]/mixed                        8  \n" ++
        "                     |                           |                                          \n" ++
        "                     ├------------------┐        ├-----------------┐                        \n" ++
        "                     |                  |        |                 |                        \n" ++
        "                     1                  2        5                 shape: [2]/number        \n" ++
        "                                                                   |                        \n" ++
        "                                                                   ├------------------┐     \n" ++
        "                                                                   |                  |     \n" ++
        "                                                                   6                  7     \n",
  % ?debugFmt("in nested_tree_print_test_~nExp:~n~ts~nGot:~n~ts~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

let_tree_print_test_() ->
  Code = "A ← 1",
  #comment{msg = Got} = print(Code),
  Exp = "let_op     \n" ++
        "|          \n" ++
        "├-------┐  \n" ++
        "|       |  \n" ++
        "'A_0'   1  \n",
  % ?debugFmt("in nested_tree_print_test_~nExp:~n~ts~nGot:~n~ts~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

double_roof_test_() ->
  Code = "(1 (2 (3 4))) 5",
  #comment{msg = Got} = print(Code),
  Exp = "shape: [2]/mixed                                             \n" ++
        "|                                                            \n" ++
        "├---------------------------------------------------------┐  \n" ++
        "|                                                         |  \n" ++
        "shape: [2]/mixed                                          5  \n" ++
        "|                                                            \n" ++
        "├-----------------┐                                          \n" ++
        "|                 |                                          \n" ++
        "1                 shape: [2]/mixed                           \n" ++
        "                  |                                          \n" ++
        "                  ├-----------------┐                        \n" ++
        "                  |                 |                        \n" ++
        "                  2                 shape: [2]/number        \n" ++
        "                                    |                        \n" ++
        "                                    ├------------------┐     \n" ++
        "                                    |                  |     \n" ++
        "                                    3                  4     \n",
  % ?debugFmt("in double_roof_test_~nExp:~n~ts~nGot:~n~ts~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

simple_function_test_() ->
  Code = "- 1 3 5",
  #comment{msg = Got} = print(Code),
  Exp = "-                        \n" ++
        "|                        \n" ++
        "|                        \n" ++
        "|                        \n" ++
        "shape: [3]/number        \n" ++
        "|                        \n" ++
        "├------------------┬--┐  \n" ++
        "|                  |  |  \n" ++
        "1                  3  5  \n",
  % ?debugFmt("in simple_function_test_~nExp:~n~ts~nGot:~n~ts~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

simple_function_II_test_() ->
  Code = "2 3 - 1 3 ",
  #comment{msg = Got} = print(Code),
  Exp = "-                                           \n" ++
        "|                                           \n" ++
        "├---------------------┐                     \n" ++
        "|                     |                     \n" ++
        "shape: [2]/number     shape: [2]/number     \n" ++
        "|                     |                     \n" ++
        "├------------------┐  ├------------------┐  \n" ++
        "|                  |  |                  |  \n" ++
        "2                  3  1                  3  \n",
  % ?debugFmt("in simple_function_II_test_~nExp:~n~ts~nGot:~n~ts~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

simple_function_III_test_() ->
  Code = "2 (3 4) - 1",
  #comment{msg = Got} = print(Code),
  Exp = "-                                          \n" ++
        "|                                          \n" ++
        "├---------------------------------------┐  \n" ++
        "|                                       |  \n" ++
        "shape: [2]/mixed                        1  \n" ++
        "|                                          \n" ++
        "├-----------------┐                        \n" ++
        "|                 |                        \n" ++
        "2                 shape: [2]/number        \n" ++
        "                  |                        \n" ++
        "                  ├------------------┐     \n" ++
        "                  |                  |     \n" ++
        "                  3                  4     \n",
  % ?debugFmt("in simple_function_III_test_~nExp:~n~ts~nGot:~n~ts~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

simple_function_IV_test_() ->
  Code = "(2 3) 4 - 1",
  #comment{msg = Got} = print(Code),
  Exp = "-                           \n" ++
        "|                           \n" ++
        "├------------------------┐  \n" ++
        "|                        |  \n" ++
        "shape: [2]/mixed         1  \n" ++
        "|                           \n" ++
        "├---------------------┐     \n" ++
        "|                     |     \n" ++
        "shape: [2]/number     4     \n" ++
        "|                           \n" ++
        "├------------------┐        \n" ++
        "|                  |        \n" ++
        "2                  3        \n",
  % ?debugFmt("in simple_function_IV_test_~nExp:~n~ts~nGot:~n~ts~n", [Exp, Got]),
  ?_assertEqual(Exp, Got).

 
%%%
%%% Helper functions
%%%

make_cells(Code) ->
  [AST] = pometo:parse_TEST(Code),
  Structure = pometo_stdlib:get_tree_TEST(AST),
  InitialStruct = [Structure#printable_tree{}],
  % its a pain but structure_to_cells returns an unreversed list
  % it makes our life easier in constructing the offsets if
  % the list is the right way around...
  {_, Cells} = pometo_stdlib:structure_to_cells_TEST(InitialStruct, 1, 1, 1,[]),
  lists:sort(Cells).

make_sizes(Code) ->
  Cells = make_cells(Code),
  {_Rows, _NoCols} = pometo_stdlib:printsize_TEST(Cells).

make_offsets(Code) ->
  Cells = make_cells(Code),
  {Cols, _NoRows} = pometo_stdlib:printsize_TEST(Cells),
  pometo_stdlib:add_lines_TEST(Cells, Cols).

print(Code) ->
  [AST] = pometo:parse_TEST(Code),
  pometo_stdlib:print_trees(AST).
