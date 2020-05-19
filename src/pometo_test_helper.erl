-module(pometo_test_helper).

-export([run/2]).

-include_lib("eunit/include/eunit.hrl").

run(Code, Expected) when is_list(Code) andalso is_list(Expected) ->
    Got = try
        Tokens               = pometo_lexer:get_tokens(Code),
        Parsed               = parse(Tokens),
        {Results, _Bindings} = pometo_runtime:run_ast(Parsed),
        pometo_runtime:format(Results)
    catch Type:Error -> ?debugFmt("Test failed to run ~p:~p", [Type, Error]),
                        {error, "test failed to run"}
    end,
    ?_assertEqual(Expected, Got).

parse(Tokenlist) ->
    Parsed = pometo_parser:parse(Tokenlist),
    case Parsed of
        {ok,    Parse} -> Parse;
        {error, Error} -> ?debugFmt("Parser error ~p~n", [Error]),
                          "error"
    end.


