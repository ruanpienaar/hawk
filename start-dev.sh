#!/bin/sh
set -x
cd `dirname $0`
exec erl -sname hawk -config $PWD/config/sys.config -pa _build/default/lib/*/ebin -boot start_sasl -setcookie hawk -proto_dist hawk_tcp \
-eval 'ok = application:start(hawk).' -hidden

# NOT working :(
#ERL_FLAGS="-sname hawk -config config/sys.config -pa _build/default/lib/*/ebin -boot start_sasl -setcookie hawk -proto_dist hawk_tcp" ./rebar3 shell