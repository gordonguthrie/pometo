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

debug(Lable, #'$ast¯'{} = AST) ->
	io:format("~n~n>>>>>>>>>>>> ~p~n~n", [Lable]),
	?debugFmt("~n~n>>>>>>>>>>>> ~p~n~n", [Lable]),
	debug(AST),
	io:format("~n~n>>>>>>>>>>>> ~n~n", []),
	?debugFmt("~n~n>>>>>>>>>>>> ~n~n", []).

debug(#'$ast¯'{line_no = LNo,
							 char_no = CNo} = AST) ->
	Line1 = io_lib:format("In ⎕debug~n", []),
	Line2 = debug2(AST, ?INITIALINDENT),
	Breaker = lists:duplicate(79, "*") ++ "\n",
	Msg = Line1 ++ Breaker ++ Line2 ++ Breaker,
	?debugFmt("in stdlib debug~n~ts~n", [Msg]),
	io:format("in stdlib debug~n~ts~n", [Msg]),
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
	Line1  = io_lib:format(Padding ++ "from line ~p at character no ~p~n", [LNo, CNo]),
	Line2  = format_do(Do, Padding),
	Line3  = io_lib:format(Padding ++ "arguments: ~p~n", [NumArgs]),
	Line4  = print_args(Args, Indent + 1, ?EMPTY_ACCUMULATOR),
	_Lines = lists:flatten([
														Line1,
														Line2,
														Line3,
														Line4
													]).

% don't do anything to scalars
make_lazy(#'$ast¯'{do = #'$shape¯'{dimensions = 0}} = AST) ->
	AST;
% do work on unindexed arrays
make_lazy(#'$ast¯'{do = #'$shape¯'{dimensions = Type} = Shp} = AST)
	when Type /= unsized_vector ->
	NewShp = Shp#'$shape¯'{dimensions = unsized_vector},
	AST#'$ast¯'{do = NewShp};
% don't do anything to anything else
make_lazy(X) ->
	X.

make_indexed(AST)     -> pometo_runtime:make_indexed(AST).

force_indexing(AST)   -> pometo_runtime:force_index(AST, index).

force_unindexing(AST) -> pometo_runtime:force_index(AST, unindex).

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

format_do(#'$shape¯'{indexed    = Index,
										 dimensions = Dims,
										 type       = Type}, Ind) ->
	io_lib:format(Ind ++ "type: ~p (indexed:~p) with dimensions ~p~n", [Type, Index, Dims]);
format_do(Do, Ind) ->
	io_lib:format(Ind ++ "~p~n", [Do]).