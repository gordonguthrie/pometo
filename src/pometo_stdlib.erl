-module(pometo_stdlib).

-include_lib("eunit/include/eunit.hrl").

-include("runtime_include.hrl").
-include("parser_records.hrl").
-include("comments.hrl").

-export([
		 debug/1,
		 tuple/1,
		 fixedmap/1,
		 record/1
		]).

-define(INITIALINDENT, 1).
-define(INDENTSIZE,    2).

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

debug2(#'$ast¯'{op      = Op,
			    args    = Args,
			    line_no = LNo,
	            char_no = CNo}, Indent) ->
	Padding = get_padding(Indent),
	NumArgs = if
		is_list(Args) -> length(Args);
	    el/=se        -> 1
	end,
	Line1  = io_lib:format(Padding ++ "from line ~p at character no ~p~n", [LNo, CNo]),
	Line2  = format_op(Op, Padding),
	Line3  = io_lib:format(Padding ++ "arguments: ~p~n", [NumArgs]),
	Line4  = print_args(Args, Indent + 1, ?EMPTY_ACCUMULATOR),
	_Lines = lists:flatten([
								Line1,
								Line2,
								Line3,
								Line4
						    ]).

tuple(X) ->
	io:format("in stdlib tuple X is ~p~n", [X]),
	X.

fixedmap(X) ->
	io:format("in stdlib fixedmap X is ~p~n", [X]),
	X.

record(X) ->
	io:format("in stdlib record X is ~p~n", [X]),
	X.

%%
%% Internal Functions
%%

get_padding(Indent) ->lists:duplicate(Indent * ?INDENTSIZE, " ").

print_args([],                     _Indent, Acc) -> lists:reverse(Acc);
print_args([#'$ast¯'{} = Ast | T], Indent,  Acc) -> Padding = get_padding(Indent),
													Slip = Padding ++ io_lib:format("~ts~n", ["element is an $ast¯:"]),
													NewAcc = debug2(Ast, Indent + 1),
													print_args(T, Indent, [NewAcc, Slip | Acc]);
print_args([H                | T], Indent,  Acc) -> Padding = get_padding(Indent),
													NewAcc = Padding ++ io_lib:format("~p~n", [H]),
													print_args(T, Indent, [NewAcc       | Acc]).

format_op(#'$shape¯'{shaping    = Shaping,
	                 indexed    = Index,
	                 dimensions = Dims,
	                 type       = Type}, Ind) ->
	io_lib:format(Ind ++ "type: ~p (~p:~p) with dimensions ~p~n", [Type, Shaping, Index, Dims]);
format_op(Op, Ind) ->
	io_lib:format(Ind ++ "~p~n", [Op]).