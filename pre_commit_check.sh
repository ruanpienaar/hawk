#!/bin/bash
make
./rebar xref
./rebar3 dialyzer