-module(pometo_test_helper).

-export([
            run_interpreter_test/1,
            run_compiler_test/2
        ]).

-include_lib("eunit/include/eunit.hrl").

run_interpreter_test(Code) when is_list(Code) ->
    Lexed = [lex(X) || X <- Code],
    ParseFn = fun(Lx, Bindings) ->
        ?debugFmt("in ParseFn Lx is ~p~n- Bindings is ~p~n", [Lx, Bindings]),
        case Lx of
            {ok, []} ->
                "";
            {ok, Tokens} ->
                Parsed = parse(Tokens),
                ?debugFmt("helper: Parsed is ~p~n", [Parsed]),
                {ok, NewBindings} = scope_dictionary:gets(),
                ?debugFmt("helper: NewBindings is ~p~n", [NewBindings]),
                CombinedBindings = pometo_runtime:combine_bindings(Bindings, NewBindings, Code),
                ?debugFmt("helper: CombinedBindings is ~p~n", [CombinedBindings]),
                _Subs = pometo_runtime:substitute_bindings(Parsed, CombinedBindings),
                Results             = pometo_runtime:run_ast(Parsed),
                ?debugFmt("helper: Results is ~p~n", [Results]),
                FormattedResults    = pometo_runtime:format(Results),
                FormattedResults;
            {error, Errors} ->
                {pometo_runtime:format_errors(Errors), #{}}
        end
    end,
    lists:foldl(ParseFn, #{}, Lexed).

lex(Code) ->
    try
        ?debugFmt("in lex Code is ~p~n", [Code]),
        _Lexed = pometo_lexer:get_tokens(Code)
    catch Type:Errs ->
        ?debugFmt("Test ~ts failed to lex~n- with ~p:~p", [Code, Type, Errs]),
        {errors, Errs}
    end.

%run_interpreter2(Code) ->
%    try
%        ?debugFmt("Code is ~p~n", [Code]),
%        Lexed = pometo_lexer:get_tokens(Code),
%        ?debugFmt("Lexed is ~p~n", [Lexed]),
%        {Parsed, Binding} = case Lexed of
%            {ok, []} ->
%                "";
%            {ok, Tokens} ->
%                ?debugFmt("Tokens is ~p~n", [Tokens]),
%                {P, B} = parse(Tokens),
%                ?debugFmt("Parsed is ~p~n", [P]),
%                ?debugFmt("Bindings is ~p~n", [B]),
%                {P, B};
%                Subs = pometo_runtime:substitute_bindings(Parsed, Bindings),
%                Results             = pometo_runtime:run_ast(Parsed),
%                ?debugFmt("Results is ~p~n", [Results]),
%                FormattedResults    = pometo_runtime:format(Results),
%                FormattedResults;
%            {error, Errors} ->
%                {pometo_runtime:format_errors(Errors), #{}}
%            end
%    catch Type:Errs ->
%        ?debugFmt("Test ~ts failed to run in the interpreter~n- with ~p:~p", [Code, Type, Errs]),
%        {errors, Errs}
%    end.

run_compiler_test(Title, Code) when is_list(Code) ->
    [run_compiler2(Title, X) || X <- Code].

run_compiler2(Title, Code) when is_list(Code) ->
    try
        Lexed = pometo_lexer:get_tokens(Code),
        case Lexed of
            {ok, []} ->
                "";
            {ok, Tokens} ->
                Parsed = parse(Tokens),
                {ok, NewBindings} = scope_dictionary:gets(),
                {module, Mod}       = pometo_compiler:compile(Parsed, Title),
                Results             = Mod:run(),
                FormattedResults    = pometo_runtime:format(Results),
                FormattedResults;
            {error, Errors} ->
                pometo_runtime:format_errors(Errors)
            end
    catch Type:Errs ->
        ?debugFmt("Test ~ts failed to run in the compiler~n- with ~p:~p", [Code, Type, Errs]),
        {error, Errs}
    end.

parse(Tokenlist) ->
    Parsed = pometo_parser:parse(Tokenlist),
    case Parsed of
        {ok,    Parse} -> Parse;
        {error, Error} -> ?debugFmt("Parser error ~p for ~ts~n", [Error, Tokenlist]),
                          "error"
    end.


