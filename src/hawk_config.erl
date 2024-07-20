-module(hawk_config).

-export([
    connection_retries/0,
    conn_retry_wait/0
]).

connection_retries() ->
    application:get_env(hawk, ?FUNCTION_NAME, 600).

conn_retry_wait() ->
    application:get_env(hawk, ?FUNCTION_NAME, 100).
