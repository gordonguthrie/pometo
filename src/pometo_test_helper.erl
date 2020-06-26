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
        CleanCode = string:join(Code, " ⋄ "),
        ?debugFmt("Test ~p failed to run in the interpreter~n- with ~p:~p", [CleanCode, Type, Errs]),
        io_lib:format("Interpreter failed to run ~ts with ~p:~p~n", [CleanCode, Type, Errs])
    end.

run_compiler_test(Title, Code) when is_list(Code) ->
    Code2 = string:join(Code, "\n"),
    try
        pometo:compile_load_and_run_TEST(Code2, Title)
    catch Type:Errs ->
        CleanCode = string:join(Code, " ⋄ "),
        ?debugFmt("Test ~p failed to run in the compiler~n- with ~p:~p", [CleanCode, Type, Errs]),
        io_lib:format("Compiler failed to run ~ts with ~p:~p~n", [CleanCode, Type, Errs])
    end.
