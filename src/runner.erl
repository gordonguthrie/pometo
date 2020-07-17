%% coding: UTF-8\n

-module(runner).

-export([run/0]).

-include_lib("eunit/include/eunit.hrl").

-compile([{nowarn_unused_function, [{run, 1}, {run1, 1}, {run2, 1}, {run3, 1}]}]).

% 7
%    Code     = ["2 +/ 1 2 3 4"],
%    Expected = "3 5 7",

% 8
% Code     = ["A ← 3 3 ⍴ 1 2 3 4",
%    "2 +/ A"],
%    Expected = "3 5\n" ++
%    "5 3\n" ++
%    "7 5",

% 10
%  Code     = ["A ← ⍳ 24\nB ← 2 3 4 ⍴ A\n2 +/[2] B"],
%    Expected = " 6  8 10 12
%                14 16 18 20
%
%                30 32 34 36
%                38 40 42 44",

% 11
%     Code     = ["+/ 2 2 ⍴ 1 22 333 444"],
%    Expected = "23 777",

% 12
%    Code     = ["+⌿ 2 2 ⍴ 1 22 333 444"],
%    Expected = "334 466",

run() ->
	Codes = [
            "A ← ⍳ 24\nB ← 2 3 4 ⍴ A\n2 +/[2] B"
          ],
	[run1(X) || X <- Codes],
	exit(0).

run(Code) ->
  run1(Code),
  run2(Code),
  run3(Code).

% normal running
run1(Code) ->
  %Resp0 = pometo:parse_TEST(Code),
  %io:format("parse_TEST returns~n~p~n", [Resp0]),
  Resp1 = pometo:interpret_TEST(Code),
  io:format("interpret_TEST returns~n~p~n~n~ts~n", [Resp1, Resp1]),
  Resp2 = pometo:compile_load_and_run_TEST(Code, "compile_runner"),
  io:format("compile_load_and_run_TEST returns~n~ts~n", [Resp2]),
  ok.

% testing format internals
run2(Code) ->
  io:format("running ~ts~n", [Code]),
  Shape = pometo:run_for_format_TEST(Code, "format_runner"),
  io:format("run_for_format_TEST returns shape ~p~n", [Shape]),
  Segs = pometo_runtime_format:build_segments_TEST(Shape),
  io:format("Segs is ~p~n", [Segs]),
  ok.

% lazy and forcing and make_indexing testing
run3(Code) ->
  Resp = pometo:compile_load_and_run_lazy_TEST(Code, "compile_lazy_runner"),
  io:format("compile_load_and_run_lazy_TEST returns~n~ts~n", [Resp]),
  Resp1 = pometo:compile_load_and_run_indexed_TEST(Code, "compile_indexed_runner"),
  io:format("compile_load_and_run_indexed_TEST returns~n~ts~n", [Resp1]),
  Resp2 = pometo:compile_load_and_run_force_index_TEST(Code, "compile_force_index_runner"),
  io:format("compile_load_and_run_force_index_TEST returns~n~ts~n", [Resp2]),
  Resp3 = pometo:compile_load_and_run_force_unindex_TEST(Code, "compile_force_unindex_runner"),
  io:format("compile_load_and_run_force_unindex_TEST returns~n~ts~n", [Resp3]),
  ok.
