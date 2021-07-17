-module(print_tree_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("tree_printing_records.hrl").
-include("runtime_include.hrl").

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
  			 #printcell{row = 1, col = 1, width = 10, text = "shape: [2]"},
  			 #printcell{row = 2, col = 1, width = 1,  text = "1"},
  			 #printcell{row = 2, col = 2, width = 1,  text = "2"}
  			 ],
  ?_assertEqual(Exp, Got).

simple_tree_print_cells_test_() ->
	Code = "A ← 1 22",
  Got = make_cells(Code),
  Exp = [
  			 #printcell{row = 1, col = 1, width = 6,  text = "let_op"},
  			 #printcell{row = 2, col = 1, width = 5,  text = "'A_0'"},
  			 #printcell{row = 2, col = 2, width = 10, text = "shape: [2]"},
  			 #printcell{row = 3, col = 2, width = 1,  text = "1"},
  			 #printcell{row = 3, col = 3, width = 2,  text = "22"}
  			 ],
  ?_assertEqual(Exp, Got).

%%%
%%% Test the preparation of the sizes
%%%

simplest_tree_sizing_test_() ->
	Code = "1 2",
  Got = make_sizes(Code),
	Exp = {#{1 => 12, 2 => 3}, 2},
  ?_assertEqual(Exp, Got).

simple_tree_sizing_test_() ->
	Code = "A ← 1 22",
  Got = make_sizes(Code),
  Exp = {#{1 => 8, 2 => 12, 3 => 4}, 3},
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
         #printcell{row = 1, col = 1, width = 1,  x_offset = 1,  y_offset = 0, text = "|"},
         #printcell{row = 1, col = 1, width = 10, x_offset = 1,  y_offset = 1, text = "shape: [2]"},
         #printcell{row = 1, col = 1, width = 1,  x_offset = 1,  y_offset = 2, text = "|"},
         #printcell{row = 1, col = 1, width = 13, x_offset = 1,  y_offset = 3, text = "-------------"},
         #printcell{row = 2, col = 1, width = 1,  x_offset = 1,  y_offset = 4, text = "|"},
         #printcell{row = 2, col = 1, width = 1,  x_offset = 1,  y_offset = 5, text = "1"},
         #printcell{row = 2, col = 2, width = 1,  x_offset = 13, y_offset = 4, text = "|"},
         #printcell{row = 2, col = 2, width = 2,  x_offset = 13, y_offset = 5, text = "22"}
         ],
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
         #printcell{row = 1, col = 1, width = 1,  x_offset = 1,  y_offset = 0, text = "|"},
         #printcell{row = 1, col = 1, width = 6,  x_offset = 1,  y_offset = 1, text = "let_op"},
         #printcell{row = 1, col = 1, width = 1,  x_offset = 1,  y_offset = 2, text = "|"},
         #printcell{row = 1, col = 1, width = 9,  x_offset = 1,  y_offset = 3, text = "---------"},
         #printcell{row = 2, col = 1, width = 1,  x_offset = 1,  y_offset = 4, text = "|"},
         #printcell{row = 2, col = 1, width = 5,  x_offset = 1,  y_offset = 5, text = "'A_0'"},
         #printcell{row = 2, col = 2, width = 1,  x_offset = 9,  y_offset = 4, text = "|"},
         #printcell{row = 2, col = 2, width = 10, x_offset = 9,  y_offset = 5, text = "shape: [2]"},
         #printcell{row = 2, col = 2, width = 1,  x_offset = 9,  y_offset = 6, text = "|"},
         #printcell{row = 2, col = 2, width = 13, x_offset = 9,  y_offset = 7, text = "-------------"},
         #printcell{row = 3, col = 2, width = 1,  x_offset = 9,  y_offset = 8, text = "|"},
         #printcell{row = 3, col = 2, width = 1,  x_offset = 9,  y_offset = 9, text = "1"},
         #printcell{row = 3, col = 3, width = 1,  x_offset = 21, y_offset = 8, text = "|"},
         #printcell{row = 3, col = 3, width = 2,  x_offset = 21, y_offset = 9, text = "22"}
         ],
  ?_assertEqual(Exp, Got).

%%%
%%% Print Cells
%%%

simplest_tree_print_test_() ->
  Code = "1 22",
  Got = print(Code),
  Exp = "shape: [2]    \n" ++
        "|             \n" ++
        "------------- \n" ++
        "|           | \n" ++
        "1           22\n",
  ?_assertEqual(Exp, Got).

simple_tree_print_test_() ->
  Code = "A ← 1 22",
  Got = print(Code),
  Exp = "let_op                \n" ++
        "|                     \n" ++
        "---------             \n" ++
        "|       |             \n" ++
        "'A_0'   shape: [2]    \n" ++
        "        |             \n" ++
        "        ------------- \n" ++
        "        |           | \n" ++
        "        1           22\n",
  ?_assertEqual(Exp, Got).

%%%
%%% Helper functions
%%%

make_cells(Code) ->
	[AST] = pometo:parse_TEST(Code),
	Structure = pometo_stdlib:get_tree_TEST(AST),
  InitialStruct = Structure#printable_tree{row = 1, col = 1},
  % its a pain but structure_to_cells returns an unreversed list
  % it makes our life easier in constructing the offsets if
  % the list is the right way around...
  lists:reverse(pometo_stdlib:structure_to_cells_TEST(InitialStruct, [])).

make_sizes(Code) ->
	Cells = make_cells(Code),
	{_Rows, _NoCols} = pometo_stdlib:printsize_TEST(Cells).

make_offsets(Code) ->
	Cells = make_cells(Code),
	{Cols, NoRows} = pometo_stdlib:printsize_TEST(Cells),
	pometo_stdlib:add_offsets_TEST(Cells, Cols, NoRows).

print(Code) ->
  [AST] = pometo:parse_TEST(Code),
  pometo_stdlib:print_trees(AST).
