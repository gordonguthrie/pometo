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
-define(EMPTYERRORS,  []).

interpret_TEST(Str) ->
	RawLexed = lex2(Str),
	{Expressions, Bindings} = parse(RawLexed, 1, ?EMPTYRESULTS),
	% ?debugFmt("Expressions is ~p~n", [Expressions]),
	% ?debugFmt("Bindings is ~p~n", [Bindings]),
	NormalRawExprs = normalise(Expressions, ?EMPTYERRORS, ?EMPTYRESULTS),
	% ?debugFmt("NormalRawExprs is ~p~n", [NormalRawExprs]),
	case NormalRawExprs of
		{?EMPTYERRORS, Exprs}  -> interpret(Exprs);
	    {Errors,       _Exprs} -> lists:flatten(Errors)
	end.

parse_TEST(Str) ->
	RawLexed = lex2(Str),
	{Exprs, _Bindings} = parse(RawLexed, 1, ?EMPTYRESULTS),
	Exprs.

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
	case [{error, E} || {error, E} <- Lexed] of
		[]     -> compile2(RawLexed);
		Errors -> {error, Errors}
	end.

%%%
%%% Helper Functions
%%%

interpret(Exprs) ->
	RunFn = fun(Expr, {Results, Bindings}) ->
		% ?debugFmt("in RunFn Expr is ~p~n- Results is ~p~n- Bindings is ~p~n", [Expr, Results, Bindings]),
		{Results, Bindings}
	end,
	lists:foldl(RunFn, {[] , #{}}, Exprs).

compile2(Lexed) ->
	RawParsed = parse(Lexed, 1, ?EMPTYRESULTS),
	case [{error, E} || {error, E} <- RawParsed] of
		[]     -> "yabba dabba doo";
		Errors -> {error, Errors}
	end.

lex(Code, LineNo) ->
    % gotta clear the scope dictionary
    scope_dictionary:clear_all(),
	scope_dictionary:put_line_no(LineNo),
    try
        Lexed = pometo_lexer:get_tokens(Code),
        {Lexed, Code}
    catch Type:Errs ->
        ?debugFmt("Code ~ts failed to lex~n- with ~p:~p", [Code, Type, Errs]),
        {{errors, Errs}, Code}
    end.

parse([], _LineNo, Results) ->
	Bindings = scope_dictionary:get_bindings(),
	Exprs = lists:reverse(Results),
	{Exprs, Bindings};
parse([{{error, E}, _Expr} | T], LineNo,  Results) ->
	parse(T, LineNo + 1, [{error, E} | Results]);
% might have blank lines which we kept for error reporting purposes
% so we just skip them
parse([{{ok, []}, _Expr} | T], LineNo, Results) ->
	parse(T, LineNo + 1, Results);
parse([{{ok, Lexed}, Expr} | T], LineNo, Results) ->
	scope_dictionary:put_line_no(LineNo),
	case pometo_parser:parse(Lexed) of
		{error, E} ->
			Error = pometo_parser:make_err(E),
		    Msg = pometo_runtime:format_errors([Error#error{expr = Expr}]),
			parse(T, LineNo + 1, [{error, Msg} | Results]);
		Parsed ->
			NewRs = validate_parsing(Parsed, Expr),
			% there might be multiple errors so validate_parsing returns a list
			parse(T, LineNo + 1, NewRs ++ Results)
	end.

validate_parsing(Parsed, Expr) ->
	case scope_dictionary:are_current_bindings_valid() of
		true ->
			case scope_dictionary:can_bindings_be_consolidated() of
				true ->
					ReturnedBindings = scope_dictionary:get_current_bindings(),
					BindingsToBeApplied = scope_dictionary:get_bindings(),
					?debugFmt(" in validate_parsing ReturnedBindings is ~p~n", [ReturnedBindings]),
					?debugFmt(" in validate_parsing BindingsToBeApplied is ~p~n", [BindingsToBeApplied]),
					ok = scope_dictionary:consolidate_bindings(),
					TransformedParsed = apply_bindings(Parsed, BindingsToBeApplied),
					[{TransformedParsed, ReturnedBindings}];
				{false, Errors} ->
					Errs = make_duplicate_errs(Errors, Expr, ?EMPTYERRORS),
					Errs
			end;
		{false, Dups} ->
			_Errs = make_duplicate_errs(Dups, Expr, ?EMPTYERRORS)
	end.

apply_bindings(Parsed, _BindingsToBeApplied) ->
	?debugFmt("FIX UP~n", []),
	Parsed.

make_duplicate_errs([], _Expr, Errs) ->
	lists:reverse(Errs);
make_duplicate_errs([H | T], Expr, Errs) ->
	Err = pometo_parser:make_err({duplicates, H}),
	Msg = pometo_runtime:format_errors([Err#error{expr = Expr}]),
	make_duplicate_errs(T, Expr, [{error, Msg} | Errs]).

normalise([], Errs, Results) ->
	{Errs, lists:reverse(Results)};
normalise([{{ok, Lines}, Bindings} | T], Errs, Results) ->
	NormalisedLines = [{X, Bindings} || X <- Lines],
	normalise(T, Errs, NormalisedLines ++ Results);
normalise([{error, Err} | T], Errs, Results) ->
	normalise(T, Errs ++ Err, Results).
