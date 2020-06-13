%% coding: UTF-8\n

-module(runner).

-export([run/0]).

-include_lib("eunit/include/eunit.hrl").

run() ->
	Codes = [
            % "MyVariable ← 1 ¯2j¯3 ¯4j3.3 6.6J¯9 7.7j8"
            % "JAY12ab ← 1 2 3",
            % "J12ab ← 1 2 3",
            % "J12 ← 1 2 3",
            % "_JjJjJ12ab ← 1 2 3",
            % "_jjJjJ12ab ← 1 2 3",
            % "X3j4 ← 1 2 3",
            % "X5J6 ← 1 2 3"
            % "_Myvar ← 1 2 3"
            "× 1 ¯2 3"
          ],
	[run(X) || X <- Codes],
	exit(0).

run(Code) ->
<<<<<<< HEAD
  io:format("running ~ts~n", [Code]),
  Resp1 = pometo:interpret_TEST(Code),
  io:format("interpret_TEST returns ~p~n", [Resp1]),
  %Resp2 = pometo:compile_load_and_run_TEST(Code, "runner"),
  %io:format("compile_load_and_run_TEST returns ~ts~n", [Resp2]),
  ok.
=======
	  io:format("~nCode is ~p~n~n", [Code]),
    Tokens  = pometo_lexer:get_tokens(Code),
	  io:format("Tokens is ~p~n", [Tokens]),
    {Parsed, _Bindings}  = parse(Tokens),
	  io:format("Parsed is ~p~n", [Parsed]),
    Results = pometo_runtime:run_ast(Parsed),
	  io:format("Results is ~p~n", [Results]),
    FormattedResults = lists:flatten(pometo_runtime:format(Results)),
    io:format("~nFormatted results is ~p~n~n", [FormattedResults]),
    {module, _} = pometo_compiler:compile(Parsed),
    X = pometo:run(),
    io:format("X is ~p~n", [X]).

noodle() ->	make_module(bingo).

make_module(Name) ->
	Mod0 = lfe_gen:new_module(Name),
    Mod1 = lfe_gen:add_exports([[run, 0]], Mod0),
    %Mod2 = make_funcs(Params, Mod1),
%    Mod2 = lfe_gen:add_form([defun,run,[],
%           [':',pometo_runtime,monadic,
%            [list,"+",
%             [tuple,
%              [quote,liffey],
%              [tuple,[quote,'¯¯\x{2374}¯¯'],[quote,eager],[quote,false],[4]],
%              [list, 0,1,2,-1]]]]], Mod1),

    Mod2 = lfe_gen:add_form([defun, run, [], ['let', [['x🤣😀😃😄😁😆😂😅🐜😑😐😍', [list, 1, -2, 3]]], 'x🤣😀😃😄😁😆😂😅🐜😑😐😍']], Mod1),
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
>>>>>>> master
