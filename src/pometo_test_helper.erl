-module(pometo_test_helper).

-export([
            run_interpreter_test/1,
            run_compiler_test/2
        ]).

-include_lib("eunit/include/eunit.hrl").

run_interpreter_test(Code) when is_list(Code) ->
    Code2 = string:join(Code, "\n"),
    try
        pometo:interpret_TEST(Code2)
    catch Type:Errs ->
        ?debugFmt("Test ~ts failed to run in the interpreter~n- with ~p:~p", [Code, Type, Errs]),
        {errors, Errs}
    end.

run_compiler_test(Title, Code) when is_list(Code) ->
    Code2 = string:join(Code, "\n"),
    try
        pometo:compile_load_and_run_TEST(Code2, Title)
    catch Type:Errs ->
        ?debugFmt("Test ~ts failed to run in the compiler~n- with ~p:~p", [Code, Type, Errs]),
        {errors, Errs}
    end.
