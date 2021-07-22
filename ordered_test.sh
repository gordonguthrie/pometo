#!/bin/bash
set -e
set -o pipefail
# rm -rf _build/default/plugins/pometo_docs_to_tests
# rebar3 pometo_docs_to_tests

# these are pretty much smoke tests - they just check that the basics are working
rebar3 eunit --module=lexer_tests                  --verbose
rebar3 eunit --module=lexer_error_tests            --verbose
rebar3 eunit --module=complex_no_lexer_tests       --verbose
rebar3 eunit --module=complex_no_lexer_error_tests --verbose
rebar3 eunit --module=parser_tests                 --verbose
rebar3 eunit --module=parser_error_tests           --verbose

# if your formatter or indexers or vector-makers are donald ducked
# then your compiler/interpreter tests are donald ducked
rebar3 eunit --module=format_tests       --verbose
rebar3 eunit --module=index_tests        --verbose
rebar3 eunit --module=make_vectors_tests --verbose

# again this set largely checks the plumbing
# <Jimmy, son, is the whoosey correctly wired into the thingiemabob?>
rebar3 eunit --module=interpreter_tests            --verbose
rebar3 eunit --module=interpreter_error_tests      --verbose
rebar3 eunit --module=interpreter_complex_no_tests --verbose
rebar3 eunit --module=compiler_tests               --verbose
rebar3 eunit --module=compiler_error_tests         --verbose
rebar3 eunit --module=compiler_complex_no_tests    --verbose

# printing tree is a pain up the arse and while there are a big set of generated
# tests for it, it is handy to have partial testing of bits of the process
rebar3 eunit --module=stdlib_print_tree_tests --verbose

# the debug function prints details of internal representations and
# thus returns different details if forced in indexing, etc, etc is switched on
# so it is implmented manualls
rebar3 eunit --module=stdlib_debug_tests --verbose

# finally if the tests have been generated run the smoke tests
# These are actual functional tests of the end system
# selected to rattle test stuff so they tend to be small and simple
# the meaty tests are left to the individual generated test suites
rebar3 eunit --module=smoke_tests_tests --verbose
