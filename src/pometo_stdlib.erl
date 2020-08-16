-module(pometo_stdlib).

-include_lib("eunit/include/eunit.hrl").

-include("runtime_include.hrl").
-include("parser_records.hrl").
-include("comments.hrl").

-export([
     debug/1,
     debug/2,
     make_lazy/1,
     make_indexed/1,
     force_indexing/1,
     force_unindexing/1
    ]).

-define(INITIALINDENT, 1).
-define(INDENTSIZE,    2).

-define(PRINTTYPE, io_format).
% -define(PRINTTYPE, debugFmt).

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
               args    = Args,
               char_no = CNo,
               line_no = LNo} = AST) when Type == func       orelse
                                          Type == maybe_func ->
  Line1  = io_lib:format("In ⎕debug_fn~n", []),
  Line2  = io_lib:format("This function array will be resolved at runtime\n", []),
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
  Msg = Line1 ++ make_breaker() ++ 
        Line2 ++ make_breaker() ++
        "as right associative this is:\n" ++
        RightS ++ "\n" ++
        "as a monadic train this is:\n" ++
        MonadicS ++ "\n" ++
        "as dyadic train this is:\n" ++
        DyadicS,
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
    pometo_runtime:make_right_associative(AST)
  catch
    error: Error ->
      io:format("got error ~p~n", [Error]),
      "bingo"
  end.

try_make_train(List, Type, Operands) ->
  try
    pometo_runtime:make_train(List, Type, Operands)
  catch
    error: Error ->
      io:format("got error ~p~n", [Error]),
      Error
  end.


print_start(Lable) -> print("~n~n>>>>START>>>>>> ~p~n~n", [Lable], ?PRINTTYPE).

print_end()        -> print("~n~n>>>>END>>>>>>>> ~n~n",   [],      ?PRINTTYPE).

make_breaker() -> lists:duplicate(79, "*") ++ "\n".

print(String, Vals, io_format) -> io:format(String, Vals);
print(String, Vals, debugFmg)  -> ?debugFmt(String, Vals).

build_execution_diagram(#'$ast¯'{do   = #'$func¯'{do = Do},
                                 args = [Arg1, Arg2]} = AST) ->
  
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