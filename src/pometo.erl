-module(pometo).

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
		 interpret_TEST/1,
		 run_for_format_TEST/2,
		 compile_load_and_run_TEST/2,
		 compile_load_and_run_lazy_TEST/2,
		 compile_load_and_run_indexed_TEST/2,
		 compile_load_and_run_force_index_TEST/2,
		 compile_load_and_run_force_unindex_TEST/2
		 ]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").

-define(EMPTYACC,       []).
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
	NormalRawExprs          = normalise(Expressions, Str, ?EMPTYERRORS, ?EMPTYRESULTS),
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
	NormalRawExprs           = normalise(Expressions, Str, ?EMPTYERRORS, ?EMPTYRESULTS),
	% there are reasons we add extra new lines at the start of an error and then take the first ones away here
	% its to make the test suites work and keep the output purty for users with multiple errors
	case NormalRawExprs of
		{?EMPTYERRORS, []}    -> []; % a line with a comment only will parse to an empty list
		{?EMPTYERRORS, Exprs} -> compile_and_run_for_format2([{{run, 0, []}, Exprs}], ModuleName, Str);
		{Errors,      _Exprs} -> string:trim(lists:flatten(Errors), leading, "\n")
	end.

compile_load_and_run_force_index_TEST(Str, ModuleName) ->
	compile_load_and_run2(Str, ModuleName, force_index).

compile_load_and_run_force_unindex_TEST(Str, ModuleName) ->
	compile_load_and_run2(Str, ModuleName, force_unindex).

compile_load_and_run_indexed_TEST(Str, ModuleName) ->
	compile_load_and_run2(Str, ModuleName, indexed).

compile_load_and_run_lazy_TEST(Str, ModuleName) ->
	compile_load_and_run2(Str, ModuleName, lazy).

compile_load_and_run_TEST(Str, ModuleName) ->
	compile_load_and_run2(Str, ModuleName, pometo).

interpret_TEST(Str) ->
	scope_dictionary:clear_all(),
	RawLexed = lex2(Str),
	{Expressions, _Bindings} = parse2(RawLexed, interpreted, 1, ?EMPTYRESULTS),
	NormalRawExprs           = normalise(Expressions, Str, ?EMPTYERRORS, ?EMPTYRESULTS),
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

compile_load_and_run2(Str, ModuleName, Type) ->
	scope_dictionary:clear_all(),
	RawLexed = lex2(Str),
	{Expressions, _Bindings} = parse2(RawLexed, compiled, 1, ?EMPTYRESULTS),
	NormalRawExprs           = normalise(Expressions, Str, ?EMPTYERRORS, ?EMPTYRESULTS),
	% there are reasons we add extra new lines at the start of an error and then take the first ones away here
	% its to make the test suites work and keep the output purty for users with multiple errors
	case NormalRawExprs of
		{?EMPTYERRORS, []}      -> []; % a line with a comment only will parse to an empty list
		{?EMPTYERRORS, Exprs}   -> Exprs2 = case Type of
																	lazy          -> transform(Exprs, lazy,          ?EMPTYACC);
																	indexed       -> transform(Exprs, indexed,       ?EMPTYACC);
																	force_index   -> transform(Exprs, force_index,   ?EMPTYACC);
																	force_unindex -> transform(Exprs, force_unindex, ?EMPTYACC);
																	pometo        -> Exprs
															 end,
															 compile_and_run3([{{run, 0, []}, Exprs2}], ModuleName, Str);
			{Errors,      _Exprs} -> string:trim(lists:flatten(Errors), leading, "\n")
	end.

transform([], _, Acc) ->
	lists:reverse(Acc);
transform([#'$ast¯'{do = #'$shape¯'{}} = AST | T], Transform, Acc) ->
	Transformed = case Transform of
		lazy          -> pometo_stdlib:make_lazy([AST]);
		indexed       -> pometo_runtime:make_indexed(AST);
		force_index   -> pometo_runtime:force_index(AST, index);
		force_unindex -> index_then_force_unindex(AST)

	end,
	transform(T, Transform, [Transformed | Acc]);
transform([#'$ast¯'{args = Args} = AST | T], Transform, Acc) ->
	Transformed = case Transform of
		lazy          -> [pometo_stdlib:make_lazy([A])         || A <- Args];
		indexed       -> [pometo_runtime:make_indexed(A)       || A <- Args];
		force_index   -> [pometo_runtime:force_index(A, index) || A <- Args];
		force_unindex -> [index_then_force_unindex(A)          || A <- Args]
	end,
	transform(T, Transform, [AST#'$ast¯'{args = Transformed} | Acc]).

index_then_force_unindex(AST) ->
	NewAst = pometo_runtime:make_indexed(AST),
	pometo_runtime:force_index(NewAst, unindex).

compile_and_run_for_format2(Exprs, ModuleName, Str) ->
	case pometo_compiler:compile(Exprs, ModuleName, Str) of
		{module, Mod} -> case Mod:run() of
							{error, Err} -> Err#error{expr = Str, at_line = 1, at_char = 1};
							Results      -> Results
						 end;
		{error, Errs} -> Errs
	end.

compile_and_run3(Exprs, ModuleName, Str) ->
	% there are reasons we add extra new lines at the start of an error and then take the first ones away here
	% its to make the test suites work and keep the output purty for users with multiple errors
	case pometo_compiler:compile(Exprs, ModuleName, Str) of
		{module, Mod} -> case Mod:run() of
							{error, Err} -> FixedErr = Err#error{expr = Str},
															RunTimeErrs = pometo_runtime_format:format_errors([FixedErr]),
															string:trim(RunTimeErrs, leading, "\n");
							Results      -> Formatted = pometo_runtime_format:format(Results),
															lists:flatten(Formatted)
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
			parse2(T, Type, LineNo + 1, [Error | Results]);
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
	DoFun = case Type of
		interpreted -> fun substitute_arg/2;
		compiled    -> fun check_arg/2
	end,
	%% three clauses
	%% the first for vectors
	ProcessFn = fun(#'$ast¯'{args = Args} = P, Res) when is_list(Args)->
					Acc = {BindingsToBeApplied, ?EMPTYERRORS, ?EMPTYRESULTS},
					{_, Errors, NewArgs} = lists:foldl(DoFun, Acc, Args),
					NewRes = case Errors of
						[] -> P#'$ast¯'{args = lists:reverse(NewArgs)};
						_  -> FullErrs = [E#error{expr = Expr} || E <- Errors],
									{error, pometo_runtime_format:format_errors(FullErrs)}
					end,
					[NewRes | Res];
	%% the second for scalars
				(#'$ast¯'{args = Arg} = P, Res) ->
					Acc = {BindingsToBeApplied, ?EMPTYERRORS, ?EMPTYRESULTS},
					{_, Errors, [NewArg]} = lists:foldl(DoFun, Acc, [Arg]),
					NewRes = case Errors of
						[] -> P#'$ast¯'{args = NewArg};
						_  -> FullErrs = [E#error{expr = Expr} || E <- Errors],
									{error, pometo_runtime_format:format_errors(FullErrs)}
					end,
					[NewRes | Res];
	%% the third for errors
				(#error{} = Err, Res) ->
					[Err | Res]
				end,
	TransformedParsed = lists:foldl(ProcessFn, ?EMPTYRESULTS, Parsed),
	TransformedParsed.

check_arg(#'$ast¯'{do   = #'$shape¯'{dimensions = 0},
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
check_arg(#'$ast¯'{do   = Do,
								   args = Args}         = AST,
					{Bindings, Errors, Results}) when is_record(Do, '$shape¯') orelse
																						is_record(Do, '$func¯') ->
	Acc = {Bindings, ?EMPTYERRORS, ?EMPTYRESULTS},
	{_, Errs, NewArgs} = lists:foldl(fun check_arg/2, Acc, Args),
	NewResults = case NewArgs of
		[#'$var¯'{} = V] -> NewAST = AST#'$ast¯'{args = V},
												[NewAST | Results];
		_                -> [AST    | Results]
	end,
	NewErrs = Errs ++ Errors,
	{Bindings, NewErrs, NewResults};
check_arg(#'$var¯'{name    = Var,
									 char_no = CNo,
									 line_no = LNo} = V, {Bindings, Errors, Results}) ->
	case maps:is_key(Var, Bindings) of
		true  -> {Bindings,  Errors, [V | Results]};
		false -> Err = #error{type    = "VARIABLE NOT DEFINED",
													msg1    = unpostfix(Var),
													msg2    = "variable is not defined",
													at_line = LNo,
													at_char = CNo},
				 {Bindings, [Err | Errors], Results}
	end;
check_arg(V, {Bindings, Errors, Results}) ->
	{Bindings, Errors, [V | Results]}.

substitute_arg(#'$ast¯'{do   = #'$shape¯'{dimensions = 0} = OrigDo,
												args = Arg}                       = AST,
				 {Bindings, Errors, Results}) ->
	Acc = {Bindings, ?EMPTYERRORS, ?EMPTYRESULTS},
	{NewB, Errs, NewArgs} = substitute_arg(Arg, Acc),
	{_, Errs2, NewA2} = case NewArgs of
		[#'$ast¯'{args = #'$var¯'{}} = A1] -> substitute_arg(A1, {Bindings, Errs, []});
		[#'$ast¯'{} = A1]                  -> {NewB, Errs, A1};
		X1                                 -> {NewB, Errs, X1}
	end,
	{NewArgs2, NewDims, NewType} = case NewA2 of
		#'$ast¯'{do   = #'$shape¯'{dimensions = D,
															 type       = T},
						 args = A2}												 -> {A2, D, T};
		#'$ast¯'{do   = complex} = A4 						 -> {A4, 0, complex};
		#'$ast¯'{do   = #'$func¯'{}} = A4					 -> {A4, 0, func};
		#'$ast¯'{do   = [{apply_fn, _}]} = A4			 -> #'$ast¯'{do = App} = A4,
																									{A4, 0, App};
		[X2]																			 -> {X2, 0, get_type(X2)};
		[] ->
			{[], 0, variable} % error condition so we don't care about the result
	end,
	NewR = case NewType of
			func             -> NewA2;
			_ 							 -> NewShp = OrigDo#'$shape¯'{indexed    = is_map(NewArgs2),
																										dimensions = NewDims,
																										type       = NewType},
													AST#'$ast¯'{do   = NewShp,
																			args = NewArgs2}
	end,
	NewResults = [NewR | Results],
	NewErrs = Errs2 ++ Errors,
	{Bindings, NewErrs, NewResults};
substitute_arg(#'$ast¯'{do   = Do,
												args = Args}         = AST,
							{Bindings, Errors, Results}) when is_record(Do, '$shape¯') orelse
																								is_record(Do, '$func¯')  orelse
																								Do == defer_evaluation   orelse
																								element(1, hd(Do)) == apply_fn ->
	Acc = {Bindings, ?EMPTYERRORS, ?EMPTYRESULTS},
	{NewB, Errs, NewArgs} = lists:foldl(fun substitute_arg/2, Acc, Args),
	{_, Errs2, NewA2} = case NewArgs of
		[#'$var¯'{} = V] -> substitute_arg(V, {Bindings, Errs, []});
		_                -> {NewB, Errs, NewArgs}
	end,
	NewResults = [AST#'$ast¯'{args = lists:reverse(NewA2)} | Results],
	NewErrs = Errs2 ++ Errors,
	{Bindings, NewErrs, NewResults};
substitute_arg(#'$var¯'{name    = Var,
												char_no = CNo,
												line_no = LNo}, {Bindings, Errors, Results}) ->

	Ret = case maps:is_key(Var, Bindings) of
		true  -> Binding = maps:get(Var, Bindings),
						 Subst   = maps:get(results, Binding),
						 {_, Errs2, [NewSubst]} = substitute_arg(Subst, {Bindings, Errors, []}),
						 NewA = chose_replacement(NewSubst),
						 {ShouldMakeNewDo, MaybeVal} = case NewA of
								 #'$ast¯'{do   = defer_evaluation,
													args = InnerArgs}					-> [D] = InnerArgs,
																											 {false, D};
								 #'$ast¯'{}												  -> {true,  none};
								 N                                  -> {false, N}
						 end,
						 NewA2 = case ShouldMakeNewDo of
								false -> renumber(MaybeVal, LNo, CNo);
								true 	-> NewDo = extract_and_renumber_do(NewA, LNo, CNo),
												 NewA#'$ast¯'{do      = NewDo,
																			char_no = CNo,
																			line_no = LNo}
						 end,
						 NewR = case {NewA2, Results} of
								{L, []} when is_list(L) -> L;
								{L, R}  when is_list(L) -> L ++ R;
								{X, R}                  -> [X] ++ R
						 end,
						 {Bindings, Errs2, NewR};
		false -> Err = #error{type    = "VARIABLE NOT DEFINED",
													msg1    = unpostfix(Var),
													msg2    = "variable is not defined",
													at_char = CNo,
													at_line = LNo},
						 {Bindings, [Err | Errors], Results}
	end,
	Ret;
substitute_arg(V, {Bindings, Errors, Results}) ->
	{Bindings, Errors, [V | Results]}.

%% if the replacement is another variable return is
chose_replacement(#'$ast¯'{do   = #'$shape¯'{dimensions = 0},
													 args = #'$var¯'{}} = AST) ->
	AST;
%% if the replacement is a scalar, substitute the value
chose_replacement(#'$ast¯'{do   = #'$shape¯'{dimensions = 0},
													 args = Args}) ->
	Args;
%% if the replacement is a function, substitute the value
chose_replacement(#'$func¯'{line_no = LNo,
														char_no = CNo} = Func) ->
	#'$ast¯'{do      = Func,
					 line_no = LNo,
					 char_no = CNo};
%% if the replacement is defer_evaluation then don't evaluate it
chose_replacement(#'$ast¯'{do = defer_evaluation} = AST) ->
	AST;
%% if the replacement is a vector return that
chose_replacement(#'$ast¯'{do = #'$shape¯'{}} = AST) ->
	AST;
%% if the replacement is an expression run it and return the value
chose_replacement(AST) ->
	chose_replacement(pometo_runtime:run_ast(AST, "")).

make_duplicate_errs([], _Expr, Errs) ->
	lists:reverse(Errs);
make_duplicate_errs([H | T], Expr, Errs) ->
	Err = pometo_parser:make_err({duplicates, H}),
	Msg = pometo_runtime_format:format_errors([Err#error{expr = Expr}]),
	make_duplicate_errs(T, Expr, [{error, Msg} | Errs]).

normalise([], _Str, Errs, Results) ->
	{Errs, lists:reverse(Results)};
normalise([#error{at_line = N} = E | T], Str, Errs, Results) ->
	NewErr = E#error{expr = get_line(Str, N)},
	ErrorStr = pometo_runtime_format:format_errors([NewErr]),
	normalise(T, Str, Errs ++ [ErrorStr], Results);
normalise([{error, ErrorStr} | T], Str, Errs, Results) ->
	normalise(T, Str, Errs ++ [ErrorStr], Results);
normalise([Lines | T], Str, Errs, Results) ->
	normalise(T, Str, Errs, [Lines | Results]).

get_line(Str, N) ->
	Lines = string:split(Str, "\n", all),
	lists:nth(N, Lines).

get_type(0)                                    -> boolean;
get_type(1)                                    -> boolean;
get_type(N) when is_number(N)                  -> number;
get_type(#'$ast¯'{do = #'$shape¯'{type = T}})  -> T.

extract_and_renumber_do(#'$ast¯'{do = #'$shape¯'{} = Shp}, LNo, CNo) ->
	Shp#'$shape¯'{char_no = CNo,
								line_no = LNo};
extract_and_renumber_do(#'$ast¯'{do = complex}, _LNo, _CNo) ->
	complex;
extract_and_renumber_do(#'$ast¯'{do = #'$func¯'{} = Func}, _LNo, _CNo) ->
	Func.

renumber(#'$ast¯'{} = AST, LNo, CNo) -> AST#'$ast¯'{line_no = LNo,
																										char_no = CNo};
renumber(X, _LNo, _CNo)              -> X.