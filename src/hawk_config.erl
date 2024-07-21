-module(hawk_config).

-export([
    connection_retries/0,
    conn_retry_wait/0,
    backoff_type/0,
    backoff_wait/0,
    callbacks_async/0,
    show_callback_stacktrace/0
]).

% TODO: deprecate
connection_retries() ->
    application:get_env(hawk, ?FUNCTION_NAME, 600).

% TODO: deprecate
conn_retry_wait() ->
    application:get_env(hawk, ?FUNCTION_NAME, 100).

% fixed or {exponential, max}.
backoff_type() ->
    case application:get_env(hawk, ?FUNCTION_NAME, fixed) of
        fixed ->
            fixed;
        {exponential, MaxTimes} ->
            {exponential, _CurrentTimes = 0, MaxTimes}
    end.

backoff_wait() ->
    application:get_env(hawk, ?FUNCTION_NAME, 100).

callbacks_async() ->
    application:get_env(hawk, ?FUNCTION_NAME, false).

show_callback_stacktrace() ->
    application:get_env(hawk, ?FUNCTION_NAME, false).