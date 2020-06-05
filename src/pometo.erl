-module(pometo).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
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
-define(EMPTYARGS, []).
-define(EMPTYERRORS,  []).

interpret_TEST(Str) ->
	RawLexed = lex2(Str),
	{Expressions, _Bindings} = parse(RawLexed, 1, ?EMPTYRESULTS),
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
	RunFn = fun(Expr, _Results) ->
		%?debugFmt("in RunFn Expr is ~p~n- Results is ~p~n", [Expr, Results]),
		%% we only run the last value
		pometo_runtime:run_ast(Expr)
	end,
	lists:foldl(RunFn, ?EMPTYRESULTS, Exprs).

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
			?debugFmt("NewRs is ~p~n", [NewRs]),
			% there might be multiple errors so validate_parsing returns a list
			parse(T, LineNo + 1, NewRs ++ Results)
	end.

validate_parsing({ok, Parsed}, Expr) ->
	case scope_dictionary:are_current_bindings_valid() of
		true ->
			case scope_dictionary:can_bindings_be_consolidated() of
				true ->
					apply_bindings(Parsed, Expr);
				{false, Errors} ->
					Errs = make_duplicate_errs(Errors, Expr, ?EMPTYERRORS),
					Errs
			end;
		{false, Dups} ->
			_Errs = make_duplicate_errs(Dups, Expr, ?EMPTYERRORS)
	end.

apply_bindings(Parsed, Expr) ->
	?debugFmt("in apply_bindings Parsed is ~p~n", [Parsed]),
	BindingsToBeApplied = scope_dictionary:get_bindings(),
	ok = scope_dictionary:consolidate_bindings(),
	ApplyFn = fun(#liffey{args = Args} = P, Res) ->
		Acc = {BindingsToBeApplied, ?EMPTYERRORS, ?EMPTYRESULTS},
		{_, Errors, NewArgs} = lists:foldl(fun substitute_arg/2, Acc, Args),
		?debugFmt("in apply_bindings NewArgs is ~p~n", [NewArgs]),
		?debugFmt("in apply_bindings Errors is ~p~n", [Errors]),
		case Errors of
			[] -> [P#liffey{args = lists:reverse(NewArgs)} | Res];
			_  -> Errors
		end
	end,
	TransformedParsed = lists:foldl(ApplyFn, [], Parsed),
	?debugFmt("in apply_bindings TransformedParsed is ~p~n", [TransformedParsed]),
	case scope_dictionary:get_errors() of
		[]    -> ?debugFmt("no errors~n", []),
				 TransformedParsed;
		Errs -> ?debugFmt("got errors ~p~n", [Errs]),
				bangette
	end.

substitute_arg(#liffey{args = Args} = L, {Bindings, Errors, Results}) ->
	Acc = {Bindings, ?EMPTYERRORS, ?EMPTYRESULTS},
	{_, Errs, NewArgs} = lists:foldl(fun substitute_arg/2, Acc, Args),
	NewResults = [L#liffey{args = lists:reverse(NewArgs)} | Results],
	NewErrs = Errs ++ Errors,
	{Bindings, NewErrs, NewResults};
substitute_arg(#var{name = Var, line_no = N, char_no = C} = V, {Bindings, Errors, Results}) ->
	?debugFmt("in substitute_arg (2)", []),
	case maps:is_key(Var, Bindings) of
		true  -> {_, NewL} = maps:get(Var, Bindings),
				 #liffey{args = NewA} = NewL,
				 {Bindings, Errors, lists:reverse(NewA) ++ Results};
		false -> Err = #error{type    = "VARIABLE NOT DEFINED",
                              msg1    = Var,
                              msg2    = "variable is not defined",
                              at_line = N,
                              at_char = C},
                 ok = scope_dictionary:append_error(Err),
				 {Bindings, [Err | Errors], Results}
	end;
substitute_arg(V, {Bindings, Errors, Results}) ->
	?debugFmt("in substitute_arg (3) for ~p~n", [V]),
	{Bindings, Errors, [V | Results]}.


%	{NewParsed, NewErrs} = lists:unzip(Response),
%	?debugFmt("NewParsed is ~p~n", [NewParsed]),
%	FlatErrs = [X#error{expr = Expr} || {error, X} <- lists:flatten(NewErrs)],
%	case FlatErrs of
%		[] -> {ok,    NewParsed};
%		_  -> {error, pometo_runtime:format_errors(FlatErrs)}
%	end.

%apply_b2(#liffey{args = A} = L, Bindings, Errors) ->
%	?debugFmt("in apply_b2 (1) A is ~p~n", [A]),
%	Response = [expand_arg(X, Bindings, Errors) || X <- A],
%	?debugFmt("in apply_b2 Response is ~p~n", [Response]),
%	{NewArgs, NewErrs} = lists:unzip(Response),
%	?debugFmt("in apply_b2 NewArgs is ~p~n", [NewArgs]),
%	{L#liffey{args = NewArgs}, NewErrs};
%apply_b2(X, _Bindings, Errors) ->
%	?debugFmt("in apply_b2 (2)~n", []),
%	{X, Errors}.

%% keep looping round your liffeys back to b2 until you hit arguments
%expand_arg(#liffey{} = L, Errors) -> 
%	Ret = apply_b2(L, Errors),
%	?debugFmt("in expand_arg (1) returning ~p~n", [Ret]),
%	Ret;
%expand_arg(#var{name = Var, line_no = N, char_no = C} = V, Bindings, Errors) ->
%	case maps:is_key(Var, Bindings) of
%		true  -> {_, NewL} = maps:get(Var, Bindings),
%				 #liffey{args = NewA} = NewL,
%				 ?debugFmt("in expand_arg NewA is ~p~n", [NewA]),
%		         {NewA2, NewErrs} = apply_b2(NewA, Bindings, Errors),
%				 ?debugFmt("in expand_arg NewA2 is ~p~n", [NewA2]),
%		false -> Err = #error{type    = "VARIABLE NOT DEFINED",
%                             msg1    = Var,
%                              msg2    = "variable is not defined",
%                              at_line = N,
%                              at_char = C},
%				 {V, [{error, Err} | Errors]}
%	end;
%expand_arg(V, _Bindings, Errors) ->
%	{V, Errors}.

make_duplicate_errs([], _Expr, Errs) ->
	lists:reverse(Errs);
make_duplicate_errs([H | T], Expr, Errs) ->
	Err = pometo_parser:make_err({duplicates, H}),
	Msg = pometo_runtime:format_errors([Err#error{expr = Expr}]),
	make_duplicate_errs(T, Expr, [{error, Msg} | Errs]).

normalise([], Errs, Results) ->
	{Errs, lists:reverse(Results)};
normalise([Lines | T], Errs, Results) ->
	normalise(T, Errs, [Lines | Results]);
% sightly smelly, should really push errors to the process dictionary
normalise([{error, Err} | T], Errs, Results) ->
	normalise(T, Errs ++ Err, Results);
normalise([{{error, Err}, _Bindings} | T], Errs, Results) ->
	normalise(T, Errs ++ Err, Results);
normalise([H | T], Errs, Results) ->
	?debugFmt("in normalise H is ~p~n", [H]),
	exit(rabbi),
	normalise(T, Errs, Results).
