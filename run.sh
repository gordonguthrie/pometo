#!/bin/bash
rebar3 compile && erl -pa _build/default/lib/pometo/ebin/ -s runner run