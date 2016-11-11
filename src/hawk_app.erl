-module(hawk_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("hawk.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    hawk_sup:start_link().

stop(_State) ->
    ok.
