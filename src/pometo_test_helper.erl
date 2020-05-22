-module(pometo_test_helper).

-export([
            run_interpreter_test/1,
            run_compiler_test/2
        ]).

-include_lib("eunit/include/eunit.hrl").

run_interpreter_test(Code) when is_list(Code) ->
    try
        Tokens              = pometo_lexer:get_tokens(Code),
        {Parsed, _Bingings} = parse(Tokens),
        Results             = pometo_runtime:run_ast(Parsed),
        pometo_runtime:format(Results)
    catch Type:Error -> ?debugFmt("Test failed to run ~p:~p", [Type, Error]),
                        {error, Error}
    end.

run_compiler_test(Title, Code) when is_list(Code) ->
    try
        Tokens             = pometo_lexer:get_tokens(Code),
        {Parsed, Bindings} = parse(Tokens),
        {module, Mod}      = pometo_compiler:compile(Parsed, Title),
        Results            = Mod:run(),
        FormattedResults   = pometo_runtime:format(Results),
        FormattedResults
    catch Type:Error -> ?debugFmt("Test failed to run ~p:~p", [Type, Error]),
                        {error, Error}
    end.




parse(Tokenlist) ->
    Parsed = pometo_parser:parse(Tokenlist),
    case Parsed of
        {ok,    Parse} -> Parse;
        {error, Error} -> ?debugFmt("Parser error ~p~n", [Error]),
                          "error"
    end.


