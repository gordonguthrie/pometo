#!/bin/bash
rm src/pometo_lexer.erl
rm src/pometo_parser.erl
rebar3 compile && erl -pa _build/default/lib/pometo/ebin/ -pa _build/default/lib/base16/ebin -s runner run
#rebar3 compile && erl -pa _build/default/lib/pometo/ebin/ -pa _build/default/lib/base16/ebin -s runner noodle
