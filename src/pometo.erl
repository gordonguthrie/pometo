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
		 compile_load_and_run_TEST/2,
		 interpret_TEST/1
		 ]).

-define(EMPTYRESULTS,   []).
-define(EMPTYARGS,      []).
-define(EMPTYERRORS,    []).

compile_load_and_run_TEST(Str, ModuleName) ->
	RawLexed = lex2(Str),
	{Expressions, _Bindings} = parse2(RawLexed, compiled, 1, ?EMPTYRESULTS),
	NormalRawExprs           = normalise(Expressions, ?EMPTYERRORS, ?EMPTYRESULTS),
	case NormalRawExprs of
		{?EMPTYERRORS, Exprs} -> compile2([{{run, 0, []}, Exprs}], ModuleName);
	    {Errors,      _Exprs} -> lists:flatten(Errors)
	end.

interpret_TEST(Str) ->
	RawLexed = lex2(Str),
	{Expressions, _Bindings} = parse2(RawLexed, interpreted, 1, ?EMPTYRESULTS),
	NormalRawExprs           = normalise(Expressions, ?EMPTYERRORS, ?EMPTYRESULTS),
	case NormalRawExprs of
		{?EMPTYERRORS, Exprs} -> interpret2(Exprs);
	    {Errors,      _Exprs} -> lists:flatten(Errors)
	end.

parse_TEST(Str) ->
	RawLexed = lex2(Str),
	{Exprs, _Bindings} = parse2(RawLexed, interpreted, 1, ?EMPTYRESULTS),
	Exprs.

lex_TEST(Str) ->
	RawLexed = lex2(Str),
	{Lexed, _Lines} = lists:unzip(RawLexed),
	Lexed.

%%%
%%% Helper Functions
%%%

compile2(Exprs, ModuleName) ->
	?debugFmt("in compile2 with Exprs of ~p~n", [Exprs]),
	{module, Mod} = pometo_compiler:compile(Exprs, ModuleName),
	Results = Mod:run(),
	pometo_runtime:format(Results).

interpret2(Exprs) ->
	Resps          = [pometo_runtime:run_ast(E) || E <- Exprs],
	FormattedResps = [pometo_runtime:format(R)  || R <- Resps],
	string:join(FormattedResps, "\n").

lex2(Str) ->
	Lines = string:split(Str, "\n", all),
	Seq = lists:seq(1, length(Lines)),
	Zip = lists:zip(Lines, Seq),
	[lex3(L, N) || {L, N} <- Zip].

lex3(Code, LineNo) ->
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

parse2([], _, _LineNo, Results) ->
	Bindings = scope_dictionary:get_bindings(),
	Exprs = lists:reverse(Results),
	{Exprs, Bindings};
parse2([{{error, E}, _Expr} | T], Type, LineNo,  Results) ->
	parse2(T, Type, LineNo + 1, [{error, E} | Results]);
% might have blank lines which we kept for error reporting purposes
% so we just skip them
parse2([{{ok, []}, _Expr} | T], Type, LineNo, Results) ->
	parse2(T, Type, LineNo + 1, Results);
parse2([{{ok, Lexed}, Expr} | T], Type, LineNo, Results) ->
	scope_dictionary:put_line_no(LineNo),
	case pometo_parser:parse(Lexed) of
		{error, E} ->
			Error = pometo_parser:make_err(E),
		    Msg = pometo_runtime:format_errors([Error#error{expr = Expr}]),
			parse2(T, Type, LineNo + 1, [{error, Msg} | Results]);
		Parsed ->
			NewRs = validate_references(Parsed, Type, Expr),
			parse2(T, Type, LineNo + 1, NewRs ++ Results)
	end.

validate_references({ok, Parsed}, Type, Expr) ->
	case scope_dictionary:are_current_bindings_valid() of
		true ->
			case scope_dictionary:can_bindings_be_consolidated() of
				true            ->
					process_bindings(Parsed, Type, Expr);
				{false, Errors} ->
					Errs = make_duplicate_errs(Errors, Expr, ?EMPTYERRORS),
					Errs
			end;
		{false, Dups} ->
			_Errs = make_duplicate_errs(Dups, Expr, ?EMPTYERRORS)
	end.

process_bindings(Parsed, Type, Expr) ->
	BindingsToBeApplied = scope_dictionary:get_bindings(),
	ok = scope_dictionary:consolidate_bindings(),
	OpFun = case Type of
		interpreted -> fun substitute_arg/2;
		compiled    -> fun check_arg/2
	end,
	ProcessFn = fun(#liffey{args = Args} = P, Res) ->
					Acc = {BindingsToBeApplied, ?EMPTYERRORS, ?EMPTYRESULTS},
					{_, Errors, NewArgs} = lists:foldl(OpFun, Acc, Args),
					NewRes = case Errors of
						[] -> P#liffey{args = lists:reverse(NewArgs)};
						_  -> FullErrs = [E#error{expr = Expr} || E <- Errors],
							  {error, pometo_runtime:format_errors(FullErrs)}
					end,
					[NewRes | Res]
				end,
	_TransformedParsed = lists:foldl(ProcessFn, [], Parsed).

check_arg(#liffey{args = Args} = L, {Bindings, Errors, Results}) ->
	Acc = {Bindings, ?EMPTYERRORS, ?EMPTYRESULTS},
	{_, Errs, NewArgs} = lists:foldl(fun check_arg/2, Acc, Args),
	NewResults = [L#liffey{args = lists:reverse(NewArgs)} | Results],
	NewErrs = Errs ++ Errors,
	{Bindings, NewErrs, NewResults};
check_arg(#var{name = Var, line_no = N, char_no = C} = V, {Bindings, Errors, Results}) ->
	case maps:is_key(Var, Bindings) of
		true  -> {Bindings, Errors, [V | Results]};
		false -> Err = #error{type    = "VARIABLE NOT DEFINED",
                              msg1    = Var,
                              msg2    = "variable is not defined",
                              at_line = N,
                              at_char = C},
				 {Bindings, [Err | Errors], Results}
	end;
check_arg(V, {Bindings, Errors, Results}) ->
	{Bindings, Errors, [V | Results]}.

substitute_arg(#liffey{args = Args} = L, {Bindings, Errors, Results}) ->
	Acc = {Bindings, ?EMPTYERRORS, ?EMPTYRESULTS},
	{_, Errs, NewArgs} = lists:foldl(fun substitute_arg/2, Acc, Args),
	NewResults = [L#liffey{args = lists:reverse(NewArgs)} | Results],
	NewErrs = Errs ++ Errors,
	{Bindings, NewErrs, NewResults};
substitute_arg(#var{name = Var, line_no = N, char_no = C}, {Bindings, Errors, Results}) ->
	case maps:is_key(Var, Bindings) of
		true  -> {_, NewL} = maps:get(Var, Bindings),
				 #liffey{args = NewA} = NewL,
				 {Bindings, Errors, lists:reverse(NewA) ++ Results};
		false -> Err = #error{type    = "VARIABLE NOT DEFINED",
                              msg1    = Var,
                              msg2    = "variable is not defined",
                              at_line = N,
                              at_char = C},
				 {Bindings, [Err | Errors], Results}
	end;
substitute_arg(V, {Bindings, Errors, Results}) ->
	{Bindings, Errors, [V | Results]}.

make_duplicate_errs([], _Expr, Errs) ->
	lists:reverse(Errs);
make_duplicate_errs([H | T], Expr, Errs) ->
	Err = pometo_parser:make_err({duplicates, H}),
	Msg = pometo_runtime:format_errors([Err#error{expr = Expr}]),
	make_duplicate_errs(T, Expr, [{error, Msg} | Errs]).

normalise([], Errs, Results) ->
	{Errs, lists:reverse(Results)};
normalise([{error, Err} | T], Errs, Results) ->
	normalise(T, Errs ++ Err, Results);
normalise([Lines | T], Errs, Results) ->
	normalise(T, Errs, [Lines | Results]).
