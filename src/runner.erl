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
  io:format("running ~ts~n", [Code]),
  Resp1 = pometo:interpret_TEST(Code),
  io:format("interpret_TEST returns ~p~n", [Resp1]),
  %Resp2 = pometo:compile_load_and_run_TEST(Code, "runner"),
  %io:format("compile_load_and_run_TEST returns ~ts~n", [Resp2]),
  ok.
