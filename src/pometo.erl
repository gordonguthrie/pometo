-module(pometo).

-include_lib("eunit/include/eunit.hrl").

-include("errors.hrl").

%%%
%%% exported for testing only
%%%
-export([
		 lex_TEST/1,
		 parse_TEST/1,
		 compile_load_and_run_TEST/1,
		 interpret_TEST/1
		 ]).

-define(EMPTYRESULTS, []).
-define(EMPTYERRORS, []).

interpret_TEST(Str) ->
	RawLexed = lex2(Str),
	Expressions = parse(RawLexed, 1, ?EMPTYRESULTS),
	NormalRawExprs = normalise(Expressions, ?EMPTYERRORS, ?EMPTYRESULTS),
	case NormalRawExprs of
		{?EMPTYERRORS, Exprs}  -> interpret(Exprs);
	    {Errors,       _Exprs} -> lists:flatten(Errors)
	end.

parse_TEST(Str) ->
	RawLexed = lex2(Str),
	_Expressions = parse(RawLexed, 1, ?EMPTYRESULTS).

lex_TEST(Str) ->
	RawLexed = lex2(Str),
	{Lexed, _Lines} = lists:unzip(RawLexed),
	Lexed.

lex2(Str) ->
	Lines = string:split(Str, "\n", all),
	Seq = lists:seq(1, length(Lines)),
	Zip = lists:zip(Lines, Seq),
	[lex(L, N) || {L, N} <- Zip].

compile_load_and_run_TEST(Str) ->
	RawLexed = lex2(Str),
	{Lexed, _Lines} = lists:unzip(RawLexed),
	% ?debugFmt("Lexed is ~p~n", [Lexed]),
	case [{error, E} || {error, E} <- Lexed] of
		[]     -> compile2(RawLexed);
		Errors -> {error, Errors}
	end.

%%%
%%% Helper Functions
%%%

interpret(Exprs) ->
	RunFn = fun(Expr, {Results, Bindings}) ->
		?debugFmt("in RunFn Expr is ~p~n- Results is ~p~n- Bindings is ~p~n", [Expr, Results, Bindings]),
		{Results, Bindings}
	end,
	lists:foldl(RunFn, {[] , #{}}, Exprs).

compile2(Lexed) ->
	RawParsed = parse(Lexed, 1, ?EMPTYRESULTS),
	% ?debugFmt("RawParsed is ~p~n", [RawParsed]),
	case [{error, E} || {error, E} <- RawParsed] of
		[]     -> "yabba dabba doo";
		Errors -> {error, Errors}
	end.

lex(Code, LineNo) ->
	scope_dictionary:put_line_no(LineNo),
    try
        Lexed= pometo_lexer:get_tokens(Code),
        {Lexed, Code}
    catch Type:Errs ->
        ?debugFmt("Test ~ts failed to lex~n- with ~p:~p", [Code, Type, Errs]),
        {{errors, Errs}, Code}
    end.

parse([], _LineNo, Results) ->
	lists:reverse(Results);
parse([{{error, E}, _Expr} | T], LineNo,  Results) ->
	parse(T, LineNo + 1, [{error, E} | Results]);
parse([{{ok, Lexed}, Expr} | T], LineNo, Results) ->
	scope_dictionary:put_line_no(LineNo),
	case pometo_parser:parse(Lexed) of
		{error, E} -> Error = pometo_parser:make_err(E),
		              Msg = pometo_runtime:format_errors([Error#error{expr = Expr}]),
					  parse(T, LineNo + 1, [{error, Msg} | Results]);
		Parsed     -> scope_dictionary:print_DEBUG(),
		              parse(T, LineNo + 1, [Parsed | Results])
	end.

normalise([], Errs, Results) ->
	{Errs, lists:reverse(Results)};
normalise([{ok, Lines} | T], Errs, Results) ->
	normalise(T, Errs, Lines ++ Results);
normalise([{error, Err} | T], Errs, Results) ->
	normalise(T, Errs ++ Err, Results).
