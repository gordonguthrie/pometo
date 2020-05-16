-module(runner).

-export([
		  run/0
		]).

run() ->
	Codes = [
	         "A ← 1 2 3"
	         ],
	[run(X) || X <- Codes],
	exit(1).

run(Code) ->
	io:format("~nCode is ~p~n~n", [Code]),
    Tokens  = pometo_lexer:get_tokens(Code),
	io:format("Tokens is ~p~n", [Tokens]),
    Parsed  = parse(Tokens),
	io:format("Parsed is ~p~n", [Parsed]),
    Results = pometo_runtime:run_ast(Parsed, []),
	io:format("Results is ~p~n", [Results]),
    FormattedResults = lists:flatten(pometo_runtime:format(Results)),
    io:format("~nFormatted results is ~p~n~n", [FormattedResults]).

parse(Tokenlist) ->
    Parsed = pometo_parser:parse(Tokenlist),
    case Parsed of
        {ok,    Parse} -> Parse;
        {error, Error} -> io:format("Parser error ~p~n", [Error]),
                          "error"
    end.