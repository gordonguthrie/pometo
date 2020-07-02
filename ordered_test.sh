#!/bin/bash
set -e
set -o pipefail
# rm -rf _build/default/plugins/pometo_docs_to_tests
# rebar3 pometo_docs_to_tests
rebar3 eunit --module=lexer_tests
rebar3 eunit --module=lexer_error_tests
rebar3 eunit --module=complex_no_lexer_tests
rebar3 eunit --module=complex_no_lexer_error_tests
rebar3 eunit --module=parser_tests
rebar3 eunit --module=parser_error_tests
# if your formatter is donald ducked then your compiler/interpreter tests are donald ducked
rebar3 eunit --module=format_tests
rebar3 eunit --module=rank_tests
rebar3 eunit --module=interpreter_tests
rebar3 eunit --module=interpreter_error_tests
rebar3 eunit --module=interpreter_complex_no_tests
rebar3 eunit --module=compiler_tests
rebar3 eunit --module=compiler_error_tests
rebar3 eunit --module=compiler_complex_no_tests
# finally if the tests have been generated run the smoke tests
rebar3 eunit --module=smoke_tests_tests

