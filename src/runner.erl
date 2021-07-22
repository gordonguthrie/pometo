%% coding: UTF-8\n

-module(runner).

-export([run/0]).

-include_lib("eunit/include/eunit.hrl").

-compile([{nowarn_unused_function, [{run, 1}, {run1, 1}, {run2, 1}, {run3, 1}]}]).

run() ->
Codes = [
%  "A ← - + ÷\n" ++
%  "B ← 333 444\n" ++
%  "C ← 555\n" ++
%  "D ← 666\n" ++
%  "777 888 + C D - B A 9999 1010"
%  Expected = "10997.9667 2119.560396"
%  "A ← 1 2"
   "A ← - + ÷\n" 
   "⎕debug A"
        ],
	[run1(X) || X <- Codes],
	exit(0).

run(Code) ->
  run1(Code),
  % run2(Code),
  run3(Code).

% normal running
run1(Code) ->
  io:format("~n~n^^^^^^^^^^^^^^^^^^~nRunning~n~n~ts~n^^^^^^^^^^^^^^^^^^~n~n", [Code]),
  % Resp0 = pometo:parse_TEST(Code),
  % io:format("parse_TEST returns~n~p~n", [Resp0]),
  Resp1 = pometo:interpret_TEST(Code),
  io:format("interpret_TEST returns~n~ts~n", [Resp1]),
  % Resp2 = pometo:compile_load_and_run_TEST(Code, "compile_runner"),
  % io:format("compile_load_and_run_TEST returns~n~ts~n", [Resp2]),
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
