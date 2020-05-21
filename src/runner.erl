%% coding: UTF-8\n

-module(runner).

-export([
		  run/0,
		  noodle/0
		]).

run() ->
	Codes = [
%	         "Z🤣🤣😀😃😄😁😆😂😅🐜+K😑[&^😐¯P😍÷I😍×E😍😍😍😍 ← 1 2 3"
			 "1 2 + 3 4"
%	         "Z ← 1 2 3"r
%			 "KORYTNAČKA ← 1 2 3"
%			 "1 2 × ¯3 4",
%			 "1 2 + 3 4",
%			 "1 2 3 4 5 + 33",
%			 "1 + 22 33 44 55",
%			 "+ 0 1 2 ¯1"
	         ],
	[run(X) || X <- Codes],
	exit(98765).

run(Code) ->
	io:format("~nCode is ~p~n~n", [Code]),
    Tokens  = pometo_lexer:get_tokens(Code),
	io:format("Tokens is ~p~n", [Tokens]),
    Parsed  = parse(Tokens),
	io:format("Parsed is ~p~n", [Parsed]),
    {Results, Bindings} = pometo_runtime:run_ast(Parsed),
	%io:format("Results is ~p~n", [Results]),
	%io:format("Bindings is ~p~n", [Bindings]),
    FormattedResults = pometo_runtime:format(Results),
    %io:format("~nFormatted results is ~p~n~n", [FormattedResults]),
    ok = pometo_compiler:compile(Parsed),
    X = pometo:run(),
    io:format("X is ~p~n", [X]).

noodle() ->
	make_module(bingo).

make_module(Name) ->
	Mod0 = lfe_gen:new_module(Name),
    Mod1 = lfe_gen:add_exports([[run, 0]], Mod0),
    %Mod2 = make_funcs(Params, Mod1),
    Mod2 = lfe_gen:add_form([defun, run, [], [':', pometo_runtime, dyadic, [list, '+', [list, 1, 2], [list, 4, 2], 0]]], Mod1),
    Forms = lfe_gen:build_mod(Mod2),
    io:format("Forms is ~p~n", [Forms]),
    {ok, Name, Binary, _} = lfe_gen:compile_mod(Mod2),
    {module, Name} = code:load_binary(Name, "nofile", Binary),
    bingo:run().

make_funcs([{Param,Fs}|Ps], Mod) ->
    %% Define catch-all which generates more explicit exit value.
    CatchAll = [f,[':',erlang,error,
                   [tuple,unknown_feature,[quote,Param],f]]],
    %% Build case clauses
    Fold = fun ({Feature,Value}, Cls) ->
               [[[quote,Feature],[quote,Value]]|Cls]
           end,
    Cls = lists:foldr(Fold, [CatchAll], Ps),
    %% Build function.
    Func = [defun,Param,[f],['case',f,Cls]],
    make_funcs(Ps, lfe_gen:add_form(Func, Mod));
    make_funcs([], Mod) -> Mod.                    

parse(Tokenlist) ->
    Parsed = pometo_parser:parse(Tokenlist),
    case Parsed of
        {ok,    Parse} -> Parse;
        {error, Error} -> io:format("Parser error ~p~n", [Error]),
                          "error"
    end.