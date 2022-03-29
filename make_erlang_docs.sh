#!/bin/bash
rm docs/erlang_docs
rebar3 compile
erl -noshell -pa ebin  -run edoc_run application "pometo" "[{preprocess, true}, {application, pometo}, {dir, \"./docs/erlang_docs\"}, {todo, true}, {app_default, \"./erlang_docs\"}, {stylesheet, \"../assets/css/style.css\"}, {sort_functions, false}]"
cp docs/images/pometo_logo_for_erlang_docs.png docs/erlang_docs/erlang.png