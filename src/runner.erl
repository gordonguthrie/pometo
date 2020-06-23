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
            %"3 4⍴ 9999 (8 7) 6"
            % "2 2 ⍴ (1 22) (1 22 (333 4444)) 55555"
            "(1 22) (1 22 (333 4444)) 55555"
          ],
	[run(X) || X <- Codes],
	exit(0).

% run(Code) ->
%  io:format("running ~ts~n", [Code]),
%  Shape = pometo:run_for_format_TEST(Code, "format_runner"),
%  io:format("run_for_format_TEST returns shape ~p~n", [Shape]),
%  Segs = pometo_runtime_format:build_segments_TEST(Shape),
%  io:format("Segs is ~p~n", [Segs]),
%  Resp = pometo_runtime_format:normalise_widths_TEST(Segs),
%  io:format("Resp is ~p~n", [Resp]),
%  ok.

run(Code) ->
  % Resp0 = pometo:parse_TEST(Code),
  % io:format("parse_TEST returns ~p~n", [Resp0]),
  Resp1 = pometo:interpret_TEST(Code),
  io:format("interpret_TEST returns~n~ts~n", [Resp1]),
  % Resp2 = pometo:compile_load_and_run_TEST(Code, "compile_runner"),
  % io:format("compile_load_and_run_TEST returns ~ts~n", [Resp2]),
  ok.
