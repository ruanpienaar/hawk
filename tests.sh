#!/bin/bash
make && ./rebar3 xref && ./rebar3 eunit -c && ./rebar3 ct -c