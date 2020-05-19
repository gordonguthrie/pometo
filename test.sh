#!/bin/bash
#rm -rf _build/default/plugins/pometo_docs_to_tests
rebar3 pometo_docs_to_tests && rebar3 eunit