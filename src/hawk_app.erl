-module(hawk_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    {ok, _sup_pid} = hawk_sup:start_link().

stop(_State) ->
    ok.