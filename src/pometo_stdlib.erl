-module(pometo_stdlib).

-export([
     debug/1,
     debug/2,
     make_lazy/1,
     make_indexed/1,
     force_indexing/1,
     force_unindexing/1,
     debug_fns/1,
     print_trees/1
    ]).

%% export for testing only, not part of the API
-export([
         get_tree_TEST/1,
         structure_to_cells_TEST/2,
         printsize_TEST/1,
         add_offsets_TEST/3
        ]).

-include_lib("eunit/include/eunit.hrl").

-include("runtime_include.hrl").
-include("parser_records.hrl").
-include("tree_printing_records.hrl").
-include("errors.hrl").
-include("comments.hrl").

-define(INITIALINDENT, 1).
-define(INDENTSIZE,    2).

-define(PRINTTYPE, io_format).
% -define(PRINTTYPE, debugFmt).

-define(ASCII_BLANK, 32).

-define(VPADDING, 3).
-define(HPADDING, 2).

%%% Testing exports

get_tree_TEST(X)              -> get_tree(X).
structure_to_cells_TEST(X, Y) -> structure_to_cells(X, Y).
printsize_TEST(X)             -> printsize(X).
add_offsets_TEST(X, Y, Z)      -> add_offsets(X, Y, Z).

print_trees(List) when is_list(List) ->
  [print_trees(X) || X <- List];
print_trees(#'$ast¯'{} = AST) ->
  Structure = get_tree(AST),
  InitialStruct = Structure#printable_tree{row = 1, col = 1},
  % its a pain but structure_to_cells returns an unreversed list
  % it makes our life easier in constructing the offsets if
  % the list is the right way around...
  Cells = lists:reverse(structure_to_cells(InitialStruct, [])),
  {Cols, NoRows} = printsize(Cells),
  % the first element is a 0th vertical line which we don't want so discard it
  [_Discard | OffsetCells] = add_offsets(Cells, Cols, NoRows),
  Blank = make_blank(Cols, NoRows),
  _PrintOutput = fit_to_blank(OffsetCells, NoRows, Blank).

fit_to_blank([], NoRows, Print) ->
  TotalRows = NoRows + ((NoRows - 1) * ?VPADDING),
  flatten(Print, TotalRows, []);
fit_to_blank([H | T], NoRows, Print) ->
  #printcell{x_offset = X, y_offset = Y, width = W, text = Txt} = H,
  NewPrint = update(Print, X, Y, W, Txt),
  fit_to_blank(T, NoRows, NewPrint).

flatten(_Print, 0, Output) ->
  lists:flatten(Output);
flatten(Print, N, Output) when N > 0 ->
  Row = maps:get(N, Print) ++ "\n",
  flatten(Print, N - 1, [Row | Output]).

update(Print, X, Y, W, Txt) ->
  Row = maps:get(Y, Print),
  NewRow = update_row(Row, X, W, Txt),
  _NewPrint = maps:put(Y, NewRow, Print).

update_row(Row, X, W, Txt) ->
  {Prefix,   Right}    = lists:split(X - 1, Row),
  {_Discard, Trailing} = lists:split(W, Right),
  Prefix ++ Txt ++ Trailing.

add_offsets(Cells, Cols, NoRows) ->
  TotalRows = NoRows + ((NoRows - 1) * ?VPADDING),
  Offsetted = add_offsets2(Cells, Cols, 1, 1, 1, 1, []),
  Stripped = [Cell || #printcell{y_offset = Y} = Cell <- Offsetted, Y =< TotalRows],
  Stripped.

add_offsets2([], _, _, _, _, _, Acc) ->
  lists:reverse(Acc);
% we have skipped a column but we need to bump the width
% and throw the cell back on the frame
add_offsets2([#printcell{row = R,
                         col = C} = H | T], Cols, R, C1, XOffset, YOffset, Acc) when C1 /= C ->
  Width = maps:get(C1, Cols),
  NewXOffset = XOffset + Width,
  add_offsets2([H | T], Cols, R, C1 + 1, NewXOffset, YOffset, Acc);
add_offsets2([#printcell{row = R,
                         col = C} = H | T], Cols, R, C, XOffset, YOffset, Acc) ->
  Width = maps:get(C, Cols),
  NewXOffset = XOffset + Width,
  NewH = H#printcell{x_offset = XOffset,
                     y_offset = YOffset},
  Vertical1  = NewH#printcell{y_offset = YOffset - 1, width = 1, text = "|"},
  Vertical2  = NewH#printcell{y_offset = YOffset + 1, width = 1, text = "|"},
  HWidth = Width + ?HPADDING - 1,
  HText = lists:flatten(lists:duplicate(HWidth, "-")),
  Horizontal = NewH#printcell{y_offset = YOffset + 2, width = HWidth, text = HText},
  NewAcc = case has_child(C, T) of
      true  -> [Horizontal, Vertical2, NewH, Vertical1 | Acc];
      false -> [NewH, Vertical1 | Acc]
  end,
  add_offsets2(T, Cols, R, C + 1, NewXOffset, YOffset, NewAcc);
% we have ended a row, so:
% * reset the column (ie X) offset to 1
% * set the column to 1
% * bump the current column no
% and throw back into the sea
add_offsets2([H | T], Cols, R, _C, _XOffset, YOffset, Acc) ->
  NewYOffset = YOffset + 1 + ?VPADDING,
  % we have added an extra line for the last element, chuck it away
  add_offsets2([H | T], Cols, R + 1, 1, 1, NewYOffset, Acc).

%% this is so fugly
has_child(Col, Tail) ->
  case [Cell || #printcell{col = C} = Cell <- Tail, C == Col] of
    [] -> false;
    _  -> true
  end.

make_blank(Cols, NoRows) ->
  Cols2 = maps:to_list(Cols),
  Width = total(Cols2, 0) - ?HPADDING,
  BlankRow = lists:duplicate(Width, ?ASCII_BLANK),
  TotalRows = NoRows + ((NoRows - 1) * ?VPADDING),
  make_blank2(TotalRows, BlankRow, #{}).

make_blank2(0, _BlankRow, Blank) -> Blank;
make_blank2(N, BlankRow,  Blank) when N >= 0 ->
  NewBlank = maps:put(N, BlankRow, Blank),
  make_blank2(N - 1, BlankRow, NewBlank).

total([],           Size) -> Size;
total([{_, N} | T], Size) -> total(T, Size + N).

printsize(List) -> printsize2(List, #{}, 0).

printsize2([], ColSizes, NoRows) ->
  {ColSizes, NoRows};
printsize2([H | T], ColSizes, NoRows) ->
  #printcell{row      = Row,
             col      = Col,
             width    = Width} = H,
  NewColSizes = get_max(Col, Width + ?HPADDING, ColSizes),
  NewNoRows = if
                NoRows >  Row -> NoRows;
                NoRows =< Row -> Row
              end,
  printsize2(T, NewColSizes, NewNoRows).

get_max(Key, Value, Map) ->
  case maps:is_key(Key, Map) of
    false -> Map#{Key => Value};
    true  -> OldMax = maps:get(Key, Map),
             if
                Value >  OldMax -> Map#{Key => Value};
                Value =< OldMax -> Map
             end
  end.

structure_to_cells(#printable_tree{root   = Root,
                                   leaves = Leaves,
                                   row    = Rw,
                                   col    = Cl}, Acc) ->
  Width = length(Root),
  Cell = #printcell{row    = Rw,
                    col    = Cl,
                    width  = Width,
                    text   = Root},
  NewAcc = [Cell | Acc],
  size_row(Leaves, Rw + 1, Cl, NewAcc);
structure_to_cells(_Leaf, Acc) ->
  Acc.

size_row([], _Rw, _Cl, Acc) ->
  Acc;
size_row([#printable_tree{} = H | T], Rw, Cl, Acc) ->
  NewH = H#printable_tree{row = Rw, col = Cl},
  NewAcc = structure_to_cells(NewH, Acc),
  size_row(T, Rw, Cl + 1, NewAcc).

get_tree(L) when is_list(L) ->
  [get_tree(X) || X <- L];
get_tree(#'$ast¯'{do   = Do,
                  args = Args}) ->
  Leaves = get_tree(Args),
  NewLeaves = case Leaves of
      L when is_list(L) -> L;
      X                 -> [X]
  end,
  #printable_tree{root   = printout(Do),
                  leaves = NewLeaves};
get_tree(N) when is_number(N) ->
  Root = lists:flatten(io_lib:format("~w", [N])),
  #printable_tree{root = Root};
get_tree(X) ->
  Root = printout(X),
  #printable_tree{root = Root}.

printout(#'$func¯'{do = Do}) ->
  lists:flatten(io_lib:format("~p", [Do]));
printout([{apply_fn, {Module, Function}}]) ->
  lists:flatten(io_lib:format("apply ~p:~p", [Module, Function]));
printout(#'$shape¯'{dimensions = Dims}) ->
  lists:flatten(io_lib:format("shape: ~p", [Dims]));
printout(A) when is_atom(A) ->
  lists:flatten(io_lib:format("~p", [A])).

debug(Lable, Contents) ->
  print_start(Lable),
  debug(Contents),
  print_end().

debug(List) when is_list(List) ->
  [debug(X) || X <- List];
debug(#'$ast¯'{do      = #'$func¯'{},
               char_no = CNo,
               line_no = LNo} = AST) ->
  Line1     = io_lib:format("In ⎕debug_fn~n", []),
  Structure = build_execution_diagram(AST),
  Msg      = Line1 ++ make_breaker() ++ 
             Structure,
  #comment{msg     = Msg,
           at_line = LNo,
           at_char = CNo};
debug(#'$ast¯'{do      = #'$shape¯'{type = Type},
               char_no = CNo,
               line_no = LNo} = AST) when Type == func       orelse
                                          Type == maybe_func ->
  Line1  = io_lib:format("In ⎕debug_fn~n", []),
  Line2  = io_lib:format("This function array will be resolved at runtime\n", []),
  Rest = debug_fns(AST),
  Msg = Line1 ++ make_breaker() ++ 
        Line2 ++ make_breaker() ++
        Rest,
  #comment{msg     = Msg,
           at_line = LNo,
           at_char = CNo};
debug(#'$ast¯'{line_no = LNo,
               char_no = CNo} = AST) ->
  Line1 = io_lib:format("In ⎕debug~n", []),
  Line2 = debug2(AST, ?INITIALINDENT),
  Msg = Line1 ++ make_breaker() ++ Line2 ++ make_breaker(),
  print("in stdlib debug~n~ts~n", [Msg], ?PRINTTYPE),
  #comment{msg     = Msg,
           at_line = LNo,
           at_char = CNo}.

debug_fns(#'$ast¯'{do      = #'$shape¯'{type = Type},
                   args    = Args} = AST) when Type == func       orelse
                                               Type == maybe_func ->
  Right  = try_make_right_associative(AST),
  RightS = build_execution_diagram(Right),
  A = #'$ast¯'{do   = #'$shape¯'{dimensions = 0},
               args = {placeholder, "⍺"}},
  W = #'$ast¯'{do   = #'$shape¯'{dimensions = 0},
               args = {placeholder, "⍵"}},
  Monadic  = try_make_train(Args, monadic, [W]),
  MonadicS = build_execution_diagram(Monadic),
  Dyadic   = try_make_train(Args, dyadic,  [A, W]),
  DyadicS  = build_execution_diagram(Dyadic),
  _Msg = "As right associative this is:" ++ "\n" ++
         RightS                          ++ "\n" ++
         "As a monadic train this is:"   ++ "\n" ++
         MonadicS                        ++ "\n" ++
         "As dyadic train this is:"      ++ "\n" ++
         DyadicS                         ++ "\n" ++
         "Where ⍺ is the LHS argument and ⍵ the RHS -".

debug2(#'$ast¯'{do      = Do,
                args    = Args,
                line_no = LNo,
                char_no = CNo}, Indent) ->
  Padding = get_padding(Indent),
  NumArgs = if
    is_list(Args) -> length(Args);
    el/=se        -> 1
  end,
  NewArgs = if
                is_list(Args) -> Args;
                el/=se        -> [Args]
  end,
  Line1  = io_lib:format(Padding ++ "from line ~p at character no ~p~n", [LNo, CNo]),
  Line2  = format_do(Do, Padding),
  Line3  = io_lib:format(Padding ++ "arguments: ~p~n", [NumArgs]),
  Line4  = print_args(NewArgs, Indent + 1, ?EMPTY_ACCUMULATOR),
  _Lines = lists:flatten([
                            Line1,
                            Line2,
                            Line3,
                            Line4
                          ]).

try_make_right_associative(AST) ->
  try
    pometo_runtime:make_runtime_right_associative(AST)
  catch
    _Type: Error ->
      case Error of
        {error, #error{} = Err} -> format(Err);
        _                       -> Error
      end
  end.

try_make_train(List, Type, Operands) ->
  try
    pometo_runtime:make_train(List, Type, Operands)
  catch
    _Type: Error ->
      case Error of
        {error, #error{} = Err} -> format(Err);
        _                       -> Error
      end
  end.

format(#error{type = Type,
              msg1 = Msg1,
              msg2 = Msg2}) -> lists:flatten(io_lib:format("~s [~s: ~s ]", [Type, Msg1, Msg2])).

print_start(Lable) -> print("~n~n>>>>START>>>>>> ~p~n~n", [Lable], ?PRINTTYPE).

print_end()        -> print("~n~n>>>>END>>>>>>>> ~n~n",   [],      ?PRINTTYPE).

make_breaker() -> lists:duplicate(79, "*") ++ "\n".

print(String, Vals, io_format) -> io:format(String, Vals);
print(String, Vals, debugFmg)  -> ?debugFmt(String, Vals).

build_execution_diagram(#'$ast¯'{do   = #'$func¯'{do = Do},
                                 args = [Arg1, Arg2]}) ->
  
  Doos  = string:join([io_lib:format("~ts", [X]) || X <- Do], " "),
  LHS  = build_execution_diagram(Arg1),
  RHS  = build_execution_diagram(Arg2),
  lists:flatten(io_lib:format("(~ts ~ts ~ts)", [LHS, maybe_wrap(Doos), RHS]));
build_execution_diagram(#'$ast¯'{do   = #'$func¯'{do = Do},
                                 args = [Arg]}) ->
  RHS = build_execution_diagram(Arg),
  lists:flatten(io_lib:format("(~ts ~ts)", [maybe_wrap(Do), RHS]));
build_execution_diagram(#'$ast¯'{do   = #'$func¯'{do = Do},
                                 args = []}) ->
  lists:flatten(io_lib:format("(~ts)", [maybe_wrap(Do)]));
build_execution_diagram(#'$ast¯'{do   = #'$shape¯'{},
                                 args = Args}) ->
  {NewArgs, Format} = case is_list(Args) of
    true  -> {Args,   "(~ts)"};
    false -> {[Args], "~ts"}
  end,
  NewArgs2 = string:join([build_execution_diagram(X) || X <- NewArgs], " "),
  lists:flatten(io_lib:format(Format, [NewArgs2]));
build_execution_diagram({placeholder, P}) ->
  lists:flatten(io_lib:format("~ts", [P]));
build_execution_diagram(X) ->
  lists:flatten(io_lib:format("~p", [X])).

maybe_wrap(Do) ->
  case length(Do) of
    1 -> Do;
    _ -> "[" ++ string:join([io_lib:format("~ts", [X]) || X <- Do], " ") ++ "]"
  end.

% don't do anything to scalars
make_lazy([#'$ast¯'{do = #'$shape¯'{dimensions = 0}} = AST]) ->
  AST;
% do work on unindexed arrays
make_lazy([#'$ast¯'{do = #'$shape¯'{dimensions = Type} = Shp} = AST])
  when Type /= unsized_vector ->
  NewShp = Shp#'$shape¯'{dimensions = unsized_vector},
  AST#'$ast¯'{do = NewShp};
% don't do anything to anything else
make_lazy([X]) ->
  X.

make_indexed([AST])     -> pometo_runtime:make_indexed(AST).

force_indexing([AST])   -> pometo_runtime:force_index(AST, index).

force_unindexing([AST]) -> pometo_runtime:force_index(AST, unindex).

%%
%% Internal Functions
%%

get_padding(Indent) ->lists:duplicate(Indent * ?INDENTSIZE, " ").

print_args([], _Indent, Acc) ->
  lists:reverse(Acc);
print_args([#'$ast¯'{} = Ast | T], Indent,  Acc) ->
  Padding = get_padding(Indent),
  Slip = Padding ++ io_lib:format("~ts~n", ["element is an $ast¯:"]),
  NewAcc = debug2(Ast, Indent + 1),
  print_args(T, Indent, [NewAcc, Slip | Acc]);
print_args([H | T], Indent,  Acc) ->
  Padding = get_padding(Indent),
  NewAcc = Padding ++ io_lib:format("~p~n", [H]),
  print_args(T, Indent, [NewAcc       | Acc]).

format_do(#'$func¯'{do             = Do,
                    type           = Type,
                    construction   = C,
                    result         = Res,
                    shape_changing = S,
                    rank           = Rank}, Ind) ->
  io_lib:format(Ind ++ "Function: ~p type: ~p construction: ~p result: ~p shape_changing: ~p rank: ~p~n",
    [Do, Type, C, Res, S, Rank]);
format_do(#'$shape¯'{indexed    = Index,
                     dimensions = Dims,
                     type       = Type}, Ind) ->
  io_lib:format(Ind ++ "Shape: type: ~p (indexed:~p) with dimensions ~p~n", [Type, Index, Dims]);
format_do(Do, Ind) ->
  io_lib:format(Ind ++ "~p~n", [Do]).