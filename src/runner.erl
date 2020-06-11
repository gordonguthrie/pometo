%% coding: UTF-8\n

-module(runner).

-export([run/0]).

run() ->
	Codes = [
%	         "Z🤣🤣😀😃😄😁😆😂😅🐜+K😑[&^😐¯P😍÷I😍×E😍😍😍😍 ← 1 2 3"
%			 "1 2 + 3 4"
%	         "1 2 + 1 2 3"
%			 "KORYTNAČKA ← 1 2 3"
%			 "1 2 × ¯3 4",
%			 "1 2 + 3 4",
%			 "1 2 3 4 5 + 33",
%			 "1 + 22 33 44 55",
%			 "+ 0 1 2 ¯1"
      "1 2 3 + 4 5 6",
      "A ← 1 2 3\nA + 5 6 7",
      "1 2 3 + 3 4",
       "A ← 1 2 3\nB ← 5 6 7\nC ← A\nD ← B\nC ÷ D"
      ],
	[run(X) || X <- Codes],
	ok.

run(Code) ->
  io:format("running ~ts~n", [Code]),
  Resp1 = pometo:interpret_TEST(Code),
  io:format("interpret_TEST returns ~ts~n", [Resp1]),
  Resp2 = pometo:compile_load_and_run_TEST(Code, "runner"),
  io:format("compile_load_and_run_TEST returns ~ts~n", [Resp2]),
  ok.
