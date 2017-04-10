-module(hawk_logger).

-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

init({}) ->
    {ok, undefined}.

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, _State) ->
    {remove_handler, {error, unknown_call}}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.