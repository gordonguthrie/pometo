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
		 interpret_TEST/1,
		 run_for_format_TEST/2
		 ]).

-define(EMPTYRESULTS,   []).
-define(EMPTYARGS,      []).
-define(EMPTYERRORS,    []).

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

run_for_format_TEST(Str, ModuleName) ->
    scope_dictionary:clear_all(),
	RawLexed = lex2(Str),
	{Expressions, _Bindings} = parse2(RawLexed, compiled, 1, ?EMPTYRESULTS),
	NormalRawExprs           = normalise(Expressions, ?EMPTYERRORS, ?EMPTYRESULTS),
	% there are reasons we add extra new lines at the start of an error and then take the first ones away here
	% its to make the test suites work and keep the output purty for users with multiple errors
	case NormalRawExprs of
		{?EMPTYERRORS, []}    -> []; % a line with a comment only will parse to an empty list
		{?EMPTYERRORS, Exprs} -> compile_and_run3([{{run, 0, []}, Exprs}], ModuleName, Str);
	    {Errors,      _Exprs} -> string:trim(lists:flatten(Errors), leading, "\n")
	end.


compile_load_and_run_TEST(Str, ModuleName) ->
    scope_dictionary:clear_all(),
	RawLexed = lex2(Str),
	{Expressions, _Bindings} = parse2(RawLexed, compiled, 1, ?EMPTYRESULTS),
	NormalRawExprs           = normalise(Expressions, ?EMPTYERRORS, ?EMPTYRESULTS),
	% there are reasons we add extra new lines at the start of an error and then take the first ones away here
	% its to make the test suites work and keep the output purty for users with multiple errors
	case NormalRawExprs of
		{?EMPTYERRORS, []}    -> []; % a line with a comment only will parse to an empty list
		{?EMPTYERRORS, Exprs} -> compile_and_run2([{{run, 0, []}, Exprs}], ModuleName, Str);
	    {Errors,      _Exprs} -> string:trim(lists:flatten(Errors), leading, "\n")
	end.

interpret_TEST(Str) ->
    scope_dictionary:clear_all(),
	RawLexed = lex2(Str),
	{Expressions, _Bindings} = parse2(RawLexed, interpreted, 1, ?EMPTYRESULTS),
	NormalRawExprs           = normalise(Expressions, ?EMPTYERRORS, ?EMPTYRESULTS),
	% there are reasons we add extra new lines at the start of an error and then take the first ones away here
	% its to make the test suites work and keep the output purty for users with multiple errors
	case NormalRawExprs of
		{?EMPTYERRORS, []}    -> []; % a line with a comment only will parse to an empty list
		{?EMPTYERRORS, Exprs} -> interpret_TEST2(Exprs, Str);
	    {Errors,      _Exprs} -> string:trim(lists:flatten(Errors), leading, "\n")
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

compile_and_run3(Exprs, ModuleName, Str) ->
	case pometo_compiler:compile(Exprs, ModuleName, Str) of
		{module, Mod} -> case Mod:run() of
							{error, Err} -> Err#error{expr = Str, at_line = 1, at_char = 1};
							Results      -> Results
						 end;
		{error, Errs} -> Errs
	end.

compile_and_run2(Exprs, ModuleName, Str) ->
	% there are reasons we add extra new lines at the start of an error and then take the first ones away here
	% its to make the test suites work and keep the output purty for users with multiple errors
	case pometo_compiler:compile(Exprs, ModuleName, Str) of
		{module, Mod} -> case Mod:run() of
							{error, Err} -> FixedErr = Err#error{expr = Str},
											RunTimeErrs = pometo_runtime_format:format_errors([FixedErr]),
											string:trim(RunTimeErrs, leading, "\n");
							Results      -> lists:flatten(pometo_runtime_format:format(Results))
						 end;
		{error, Errs} -> pometo_runtime_format:format_errors(Errs)
	end.

interpret2(Exprs, Str) ->
	Resps          = [pometo_runtime:run_ast(E, Str)  || E <- Exprs],
	FormattedResps = [pometo_runtime_format:format(R) || R <- Resps],
	FormattedResps.

%% in the TEST suite we make the interpreter mimic the compiler and return the last value only
interpret_TEST2(Exprs, Str) ->
	Resps          = [pometo_runtime:run_ast(E, Str)  || E <- Exprs],
	FormattedResps = [pometo_runtime_format:format(R) || R <- Resps],
	LastResponse   = hd(lists:reverse(FormattedResps)),
	% there are reasons we add extra new lines at the start of an error and then take the first ones away here
	% its to make the test suites work and keep the output purty for users with multiple errors
	string:trim(lists:flatten(LastResponse), leading, "\n").

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
		    Msg   = pometo_runtime_format:format_errors([Error#error{expr = Expr}]),
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
	%% two clauses
	%% the first for vectors
	ProcessFn = fun(#'$ast¯'{args = Args} = P, Res) when is_list(Args)->
					Acc = {BindingsToBeApplied, ?EMPTYERRORS, ?EMPTYRESULTS},
					{_, Errors, NewArgs} = lists:foldl(OpFun, Acc, Args),
					NewRes = case Errors of
						[] -> P#'$ast¯'{args = lists:reverse(NewArgs)};
						_  -> FullErrs = [E#error{expr = Expr} || E <- Errors],
							  {error, pometo_runtime_format:format_errors(FullErrs)}
					end,
					[NewRes | Res];
	%% the second for scalars
				(#'$ast¯'{args = Arg} = P, Res) ->
					Acc = {BindingsToBeApplied, ?EMPTYERRORS, ?EMPTYRESULTS},
					{_, Errors, [NewArg]} = lists:foldl(OpFun, Acc, [Arg]),
					NewRes = case Errors of
						[] -> P#'$ast¯'{args = NewArg};
						_  -> FullErrs = [E#error{expr = Expr} || E <- Errors],
							  {error, pometo_runtime_format:format_errors(FullErrs)}
					end,
					[NewRes | Res]
				end,
	_TransformedParsed = lists:foldl(ProcessFn, [], Parsed).

check_arg(#'$ast¯'{op   = #'$shape¯'{dimensions = 0},
				   args = Arg}                       = L,
			   {Bindings, Errors, Results}) ->
	Acc = {Bindings, ?EMPTYERRORS, ?EMPTYRESULTS},
	{NewBindings, NewErrs, NewArgs} = check_arg(Arg, Acc),
	NewA2 = case NewArgs of
		[#'$var¯'{} = V] -> V;
		[NewV]           -> NewV;
		[]               -> [] % error condition so we don't care about the result
	end,
	NewResult = L#'$ast¯'{args = NewA2},
	{NewBindings, NewErrs ++ Errors, [NewResult] ++ Results};
check_arg(#'$ast¯'{op   = #'$shape¯'{},
	               args = Args}         = L, {Bindings, Errors, Results}) ->
	Acc = {Bindings, ?EMPTYERRORS, ?EMPTYRESULTS},
	{_, Errs, NewArgs} = lists:foldl(fun check_arg/2, Acc, Args),
	NewResults = case NewArgs of
		[#'$var¯'{} = V] -> NewL = L#'$ast¯'{args = V},
		                    [NewL | Results];
		_                -> [L    | Results]
	end,
	NewErrs = Errs ++ Errors,
	{Bindings, NewErrs, NewResults};
check_arg(#'$var¯'{name    = Var,
	               char_no = CNo,
	               line_no = LNo} = V, {Bindings, Errors, Results}) ->
	case maps:is_key(Var, Bindings) of
		true  -> {Bindings,  Errors, [V | Results]};
		false -> Err = #error{type    = "VARIABLE NOT DEFINED",
                              msg1    = Var,
                              msg2    = "variable is not defined",
                              at_line = LNo,
                              at_char = CNo},
				 {Bindings,  [Err | Errors], Results}
	end;
check_arg(V, {Bindings, Errors, Results}) ->
	{Bindings, Errors, [V | Results]}.

substitute_arg(#'$ast¯'{op   = #'$shape¯'{dimensions = 0} = OrigOp,
					    args = Arg}                       = L,
			   {Bindings, Errors, Results}) ->
	Acc = {Bindings, ?EMPTYERRORS, ?EMPTYRESULTS},
	{NewB, Errs, NewArgs} = substitute_arg(Arg, Acc),
	{_, Errs2, NewA2} = case NewArgs of
		[#'$ast¯'{args = #'$var¯'{}} = A1] -> substitute_arg(A1, {Bindings, Errs, []});
		[#'$ast¯'{} = A1]                  -> {NewB, Errs, A1};
		X1                                 -> {NewB, Errs, X1}
	end,
	{NewArgs2, NewDims, NewType} = case NewA2 of
		#'$ast¯'{op   = #'$shape¯'{dimensions = D,
		                           type       = T},
				 args = A2} ->
			{A2, D, T};
		#'$ast¯'{op = complex} = A4 ->
			{A4, 0, complex};
		[#'$ast¯'{op   = #'$shape¯'{dimensions = D,
		                           type       = T},
				  args = A3}] ->
			{A3, D, T};
		[X2] ->
			{X2, 0, get_type(X2)};
		[] ->
			{[], 0, variable} % error condition so we don't care about the result
	end,
	NewResults = [L#'$ast¯'{op   = OrigOp#'$shape¯'{dimensions = NewDims,
	                                                type       = NewType},
	                        args = NewArgs2} | Results],
	NewErrs = Errs2 ++ Errors,
	{Bindings, NewErrs, NewResults};
substitute_arg(#'$ast¯'{op   = #'$shape¯'{},
					    args = Args}         = L,
			   {Bindings, Errors, Results}) ->
	Acc = {Bindings, ?EMPTYERRORS, ?EMPTYRESULTS},
	{NewB, Errs, NewArgs} = lists:foldl(fun substitute_arg/2, Acc, Args),
	{_, Errs2, NewA2} = case NewArgs of
		[#'$var¯'{} = V] -> substitute_arg(V, {Bindings, Errs, []});
		_                -> {NewB, Errs, NewArgs}
	end,
	NewResults = [L#'$ast¯'{args = lists:reverse(NewA2)} | Results],
	NewErrs = Errs2 ++ Errors,
	{Bindings, NewErrs, NewResults};
substitute_arg(#'$var¯'{name    = Var,
	                    char_no = CNo,
	                    line_no = LNo}, {Bindings, Errors, Results}) ->
	case maps:is_key(Var, Bindings) of
		true  -> Binding = maps:get(Var, Bindings),
				 Subst   = maps:get(results, Binding),
				 NewA = chose_replacement(Subst),
				 NewOp = extract_and_renumber_op(NewA, CNo, LNo),
				 NewA2 = NewA#'$ast¯'{op      = NewOp,
				                      char_no = CNo,
				                      line_no = LNo},
				 {Bindings, Errors,  [NewA2] ++ Results};
		false -> Err = #error{type    = "VARIABLE NOT DEFINED",
                              msg1    = Var,
                              msg2    = "variable is not defined",
                              at_char = CNo,
                              at_line = LNo},
				 {Bindings, [Err | Errors], Results}
	end;
substitute_arg(V, {Bindings, Errors, Results}) ->
	{Bindings, Errors, [V | Results]}.

%% if the replacement is another variable return is
chose_replacement(#'$ast¯'{op   = #'$shape¯'{dimensions = 0},
	                       args = #'$var¯'{}} = A) ->
	A;
%% if the replacement is a scalar, substitute the value
chose_replacement(#'$ast¯'{op   = #'$shape¯'{dimensions = 0},
	                       args = Args}) ->
	Args;
%% if the replacement is a vector return that
chose_replacement(#'$ast¯'{op   = #'$shape¯'{}} = A) ->
	A;
%% if the replacement is an expression run it and return the value
chose_replacement(AST) ->
	chose_replacement(pometo_runtime:run_ast(AST, "")).

make_duplicate_errs([], _Expr, Errs) ->
	lists:reverse(Errs);
make_duplicate_errs([H | T], Expr, Errs) ->
	Err = pometo_parser:make_err({duplicates, H}),
	Msg = pometo_runtime_format:format_errors([Err#error{expr = Expr}]),
	make_duplicate_errs(T, Expr, [{error, Msg} | Errs]).

normalise([], Errs, Results) ->
	{Errs, lists:reverse(Results)};
normalise([{error, Err} | T], Errs, Results) ->
	normalise(T, Errs ++ Err, Results);
normalise([Lines | T], Errs, Results) ->
	normalise(T, Errs, [Lines | Results]).

get_type(0)                                    -> boolean;
get_type(1)                                    -> boolean;
get_type(N) when is_number(N)                  -> number;
get_type(#'$ast¯'{op = #'$shape¯'{type = T}})  -> T.


extract_and_renumber_op(#'$ast¯'{op = #'$shape¯'{} = Shp}, CNo, LNo) ->
	Shp#'$shape¯'{char_no = CNo,
				  line_no = LNo};
extract_and_renumber_op(#'$ast¯'{op = complex}, _CNo, _LNo) ->
	complex.
