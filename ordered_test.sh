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
rebar3 eunit --module=interpreter_tests
rebar3 eunit --module=interpreter_error_tests
rebar3 eunit --module=interpreter_complex_no_tests
rebar3 eunit --module=compiler_tests
rebar3 eunit --module=compiler_error_tests
rebar3 eunit --module=compiler_complex_no_tests
rebar3 eunit --module=format_tests

