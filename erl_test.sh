#!/bin/sh

erlc geonames_test.erl
erl -noshell -s geonames_test main -s init stop
