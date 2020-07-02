-module(pometo_runtime_monadic_op).

-export([
					monadic_op_RUNTIME/1
				]).

-include_lib("eunit/include/eunit.hrl").

-include("parser_records.hrl").
-include("errors.hrl").
-include("runtime_include.hrl").

monadic_op_RUNTIME(AST) -> io:format("in monadic_op_RUNTIME with ~p~n", [AST]),
													 AST.
