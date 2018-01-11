-module(hawk_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->

    % dbg:tracer(),
    % dbg:p(all, call),
    % dbg:tpl(net_kernel, connect, cx),


    hawk_sup:start_link().

stop(_State) ->
    ok.
