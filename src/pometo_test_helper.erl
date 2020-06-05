-module(pometo_test_helper).

-export([
            run_interpreter_test/1,
            run_compiler_test/2
        ]).

-include_lib("eunit/include/eunit.hrl").

run_interpreter_test(Code) when is_list(Code) ->
    try
        R = pometo:interpret_TEST(Code),
        ?debugFmt("R is ~p~n", [R]),
        R
    catch Type:Errs ->
        ?debugFmt("Test ~ts failed to run in the interpreter~n- with ~p:~p", [Code, Type, Errs]),
        {errors, Errs}
    end.

run_compiler_test(Title, Code) when is_list(Code) ->
    "deefie".
%    [run_compiler2(Title, X) || X <- Code].

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
        Errs
    end.

parse(Tokenlist) ->
    Parsed = pometo_parser:parse(Tokenlist),
    case Parsed of
        {ok,    Parse} -> Parse;
        {error, Error} -> ?debugFmt("Parser error ~p for ~ts~n", [Error, Tokenlist]),
                          "error"
    end.


