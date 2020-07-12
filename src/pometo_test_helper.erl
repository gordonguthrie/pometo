-module(pometo_test_helper).

-export([
            run_interpreter_test/1,
            run_compiler_test/2,
            run_compiler_lazy_test/2,
            run_compiler_indexed_test/2,
            run_compiler_force_index_test/2,
            run_compiler_force_unindex_test/2
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

run_compiler_lazy_test(Title, Code) when is_list(Code) ->
    Code2 = string:join(Code, "\n"),
    try
        pometo:compile_load_and_run_lazy_TEST(Code2, Title)
    catch Type:Errs ->
        CleanCode = string:join(Code, " ⋄ "),
        ?debugFmt("Test ~p failed to run lazy in the compiler~n- with ~p:~p", [CleanCode, Type, Errs]),
        io_lib:format("Compiler failed to run lazy ~ts with ~p:~p~n", [CleanCode, Type, Errs])
    end.

run_compiler_indexed_test(Title, Code) when is_list(Code) ->
    Code2 = string:join(Code, "\n"),
    try
        pometo:compile_load_and_run_indexed_TEST(Code2, Title)
    catch Type:Errs ->
        CleanCode = string:join(Code, " ⋄ "),
        ?debugFmt("Test ~p failed to run indexed in the compiler~n- with ~p:~p", [CleanCode, Type, Errs]),
        io_lib:format("Compiler failed to run indexed ~ts with ~p:~p~n", [CleanCode, Type, Errs])
    end.

run_compiler_force_index_test(Title, Code) when is_list(Code) ->
    Code2 = string:join(Code, "\n"),
    try
        pometo:compile_load_and_run_force_index_TEST(Code2, Title)
    catch Type:Errs ->
        CleanCode = string:join(Code, " ⋄ "),
        ?debugFmt("Test ~p failed to run force index in the compiler~n- with ~p:~p", [CleanCode, Type, Errs]),
        io_lib:format("Compiler failed to run force index ~ts with ~p:~p~n", [CleanCode, Type, Errs])
    end.

run_compiler_force_unindex_test(Title, Code) when is_list(Code) ->
    Code2 = string:join(Code, "\n"),
    try
        pometo:compile_load_and_run_force_unindex_TEST(Code2, Title)
    catch Type:Errs ->
        CleanCode = string:join(Code, " ⋄ "),
        ?debugFmt("Test ~p failed to run force unindex in the compiler~n- with ~p:~p", [CleanCode, Type, Errs]),
        io_lib:format("Compiler failed to run force unindex ~ts with ~p:~p~n", [CleanCode, Type, Errs])
    end.
