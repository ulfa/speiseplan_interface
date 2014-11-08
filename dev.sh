#!/bin/sh
erl -sname speiseplan_interface -setcookie nocookie -pa $PWD*/ebin $PWD/deps/*/ebin -boot start_sasl -s lager -s speiseplan_interface
