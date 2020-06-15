-module(pometo).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").

%%%
%%% production exports
%%%

-export([
		 interpret/2
		]).

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
-define(NO_DIMENSIONS,  none).

%%
%% Production API
%%

interpret(Str, ExternalBindings) ->
    scope_dictionary:clear_all(),
	% first we need to load the external bindings into the scope dictionary
	ok = scope_dictionary:persist_bindings(ExternalBindings),
	BinExpr = unicode:characters_to_binary(Str, utf8),
	RawLexed = lex2(Str),
	{Expressions, Bindings} = parse2(RawLexed, interpreted, 1, ?EMPTYRESULTS),
	NormalRawExprs          = normalise(Expressions, ?EMPTYERRORS, ?EMPTYRESULTS),
	case NormalRawExprs of
		{?EMPTYERRORS, []} ->
			{#{},      #{expr => BinExpr, succeeded => true,  results => []}}; % a line with a comment only will parse to an empty list
		{?EMPTYERRORS, Exprs} ->
			Results = unicode:characters_to_binary(string:join(interpret2(Exprs, Str), "\n"), utf8),
			{Bindings, #{expr => BinExpr, succeeded => true, results => Results}};
	    {Errors,      _Exprs} ->
            Errs = unicode:characters_to_binary(lists:flatten(Errors), utf8),
            {#{},      #{expr => BinExpr, succeeded => false, results => Errs}}
	end.

%%
%% Testing API
%%

compile_load_and_run_TEST(Str, ModuleName) ->
    scope_dictionary:clear_all(),
	RawLexed = lex2(Str),
	{Expressions, _Bindings} = parse2(RawLexed, compiled, 1, ?EMPTYRESULTS),
	NormalRawExprs           = normalise(Expressions, ?EMPTYERRORS, ?EMPTYRESULTS),
	case NormalRawExprs of
		{?EMPTYERRORS, []}    -> []; % a line with a comment only will parse to an empty list
		{?EMPTYERRORS, Exprs} -> compile_and_run2([{{run, 0, []}, Exprs}], ModuleName, Str);
	    {Errors,      _Exprs} -> lists:flatten(Errors)
	end.

interpret_TEST(Str) ->
    scope_dictionary:clear_all(),
	RawLexed = lex2(Str),
	{Expressions, _Bindings} = parse2(RawLexed, interpreted, 1, ?EMPTYRESULTS),
	NormalRawExprs           = normalise(Expressions, ?EMPTYERRORS, ?EMPTYRESULTS),
	case NormalRawExprs of
		{?EMPTYERRORS, []}    -> []; % a line with a comment only will parse to an empty list
		{?EMPTYERRORS, Exprs} -> interpret_TEST2(Exprs, Str);
	    {Errors,      _Exprs} -> lists:flatten(Errors)
	end.

parse_TEST(Str) ->
    scope_dictionary:clear_all(),
	RawLexed = lex2(Str),
	{Exprs, _Bindings} = parse2(RawLexed, interpreted, 1, ?EMPTYRESULTS),
	Exprs.

lex_TEST(Str) ->
    scope_dictionary:clear_all(),
	RawLexed = lex2(Str),
	{Lexed, _Lines} = lists:unzip(RawLexed),
	Lexed.

%%%
%%% Helper Functions
%%%

compile_and_run2(Exprs, ModuleName, Str) ->
	case pometo_compiler:compile(Exprs, ModuleName, Str) of
		{module, Mod} -> case Mod:run() of
							{error, Err} -> FixedErr = Err#error{expr = Str, at_line = 1, at_char = 1},
											pometo_runtime:format_errors([FixedErr]);
							Results      -> pometo_runtime:format(Results)
						 end;
		{error, Errs} -> pometo_runtime:format_errors(Errs)
	end.

interpret2(Exprs, Str) ->
	Resps          = [pometo_runtime:run_ast(E, Str) || E <- Exprs],
	FormattedResps = [pometo_runtime:format(R)       || R <- Resps],
	FormattedResps.

%% in the TEST suite we make the interpreter mimic the compiler and return the last value only
interpret_TEST2(Exprs, Str) ->
	Resps          = [pometo_runtime:run_ast(E, Str) || E <- Exprs],
	FormattedResps = [pometo_runtime:format(R)       || R <- Resps],
	LastResponse   = hd(lists:reverse(FormattedResps)),
	LastResponse.

lex2(Str) ->
	Lines = string:split(Str, "\n", all),
	Seq = lists:seq(1, length(Lines)),
	Zip = lists:zip(Lines, Seq),
	[lex3(L, N) || {L, N} <- Zip].

lex3(Code, LineNo) ->
    % gotta clear the scope dictionary
	scope_dictionary:put_line_no(LineNo),
    try
        Lexed = pometo_lexer:get_tokens_TEST(Code),
        {Lexed, Code}
    catch _Type:Errs ->
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
		    Msg   = pometo_runtime:format_errors([Error#error{expr = Expr}]),
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
	ProcessFn = fun(#ast{args = Args} = P, Res) ->
					Acc = {BindingsToBeApplied, ?NO_DIMENSIONS, ?EMPTYERRORS, ?EMPTYRESULTS},
					{_, _Dims, Errors, NewArgs} = lists:foldl(OpFun, Acc, Args),
					NewRes = case Errors of
						[] -> P#ast{args = lists:reverse(NewArgs)};
						_  -> FullErrs = [E#error{expr = Expr} || E <- Errors],
							  {error, pometo_runtime:format_errors(FullErrs)}
					end,
					[NewRes | Res]
				end,
	_TransformedParsed = lists:foldl(ProcessFn, [], Parsed).

check_arg(#ast{op   = #'$¯¯⍴¯¯'{dimensions = D} = Rho,
	           args = Args} = L, {Bindings, _Dims, Errors, Results}) ->
	Acc = {Bindings, D, ?EMPTYERRORS, ?EMPTYRESULTS},
	{_, NewDims, Errs, NewArgs} = lists:foldl(fun check_arg/2, Acc, Args),
	% if the arguments is a single variable it needs to be hoisted out of the list
	NewResults = case NewArgs of
		[#'$¯¯var¯¯'{} = V] -> NewRho = substitute_dims(Rho, NewDims),
							   NewL = L#ast{op   = NewRho,
		                                    args = V},
		                       [NewL | Results];
		_                   -> [L    | Results]
	end,
	NewErrs = Errs ++ Errors,
	{Bindings, D, NewErrs, NewResults};
check_arg(#'$¯¯var¯¯'{name = Var, line_no = N, char_no = C} = V, {Bindings, Dims, Errors, Results}) ->
	case maps:is_key(Var, Bindings) of
		true  -> Binding = maps:get(Var, Bindings),
				 Subst   = maps:get(results, Binding),
				 #ast{op = Op} = Subst,
				 NewDims = get_dimensions(Op),
				 {Bindings, NewDims, Errors, [V | Results]};
		false -> Err = #error{type    = "VARIABLE NOT DEFINED",
                              msg1    = Var,
                              msg2    = "variable is not defined",
                              at_line = N,
                              at_char = C},
				 {Bindings, Dims, [Err | Errors], Results}
	end;
check_arg(V, {Bindings, Dims, Errors, Results}) ->
	{Bindings, Dims, Errors, [V | Results]}.

substitute_arg(#ast{op   = #'$¯¯⍴¯¯'{dimensions = D} = Rho,
					args = Args} = L, {Bindings, Dims, Errors, Results}) ->
	Acc = {Bindings, D, ?EMPTYERRORS, ?EMPTYRESULTS},
	{NewB, NewDims, Errs, NewArgs} = lists:foldl(fun substitute_arg/2, Acc, Args),
	{_, NewD2, Errs2, NewA2} = case NewArgs of
		[#'$¯¯var¯¯'{} = V] -> substitute_arg(V, {Bindings, Dims, Errs, []});
		_                   -> {NewB, NewDims, Errs, NewArgs}
	end,
	NewRho = substitute_dims(Rho, NewD2),
	NewResults = [L#ast{op   = NewRho,
	                    args = lists:reverse(NewA2)} | Results],
	NewErrs = Errs2 ++ Errors,
	{Bindings, D, NewErrs, NewResults};
substitute_arg(#'$¯¯var¯¯'{name = Var, line_no = N, char_no = C}, {Bindings, Dims, Errors, Results}) ->
	case maps:is_key(Var, Bindings) of
		true  -> Binding = maps:get(Var, Bindings),
				 Subst   = maps:get(results, Binding),
				 #ast{op   = Op,
				      args = NewA} = Subst,
				 NewDims = get_dimensions(Op),
				 {Bindings, NewDims, Errors, lists:reverse(NewA) ++ Results};
		false -> Err = #error{type    = "VARIABLE NOT DEFINED",
                              msg1    = Var,
                              msg2    = "variable is not defined",
                              at_line = N,
                              at_char = C},
				 {Bindings, Dims, [Err | Errors], Results}
	end;
substitute_arg(V, {Bindings, Dims, Errors, Results}) ->
	{Bindings, Dims, Errors, [V | Results]}.

substitute_dims(#'$¯¯⍴¯¯'{} = Rho, Dims) -> Rho#'$¯¯⍴¯¯'{dimensions = Dims}.

get_dimensions(#'$¯¯⍴¯¯'{dimensions = D}) -> D.

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
