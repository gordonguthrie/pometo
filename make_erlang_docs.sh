#!/bin/bash
rm doc/*
erl -noshell -pa ebin  -run edoc_run application "pometo" "[{preprocess, true}]"