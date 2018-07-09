-module(hawk_node_tests).
-include_lib("eunit/include/eunit.hrl").

% Not testing any of the process code
% it is covered by hawk_SUITE.erl

hawk_node_unit_test_() ->
    unit_testing:setup(
        % Setup
        fun() -> ok end,
        % Cleanup
        fun(_) -> ok end,
        % Tests
        [
            {"hawk_node:initial_state/0",
                ?_assert(unit_testing:try_test_fun(fun initial_state/0))},
            {"hawk_node:connected_callback/1",
                ?_assert(unit_testing:try_test_fun(fun connected_callback/0))},
            {"hawk_node:disconnect_or_delete_callback/1",
                ?_assert(unit_testing:try_test_fun(fun disconnect_or_delete_callback/0))}

        ],
        % Mocks started at setup phase
        [
        ],
        % Strict meck unload
        true
    ).

initial_state() ->
    CF = fun() -> ok end,
    DCF = fun() -> ok end,
    ?assertEqual(
        #{ connected=>false,
           node=>node,
           cookie=>cookie,
           conn_cb_list=>[{con, CF}],
           disc_cb_list=>[{discon, DCF}],
           connection_retries=>600,
           conn_retry_wait=>100
        },
        hawk_node:initial_state(node, cookie, [{con, CF}], [{discon, DCF}])
    ).

connected_callback() ->
    ?assertEqual(
        ok,
        hawk_node:connected_callback([])
    ),
    ?assertEqual(
        ok,
        hawk_node:connected_callback([])
    ).

disconnect_or_delete_callback() ->
    ?assertEqual(
        ok,
        hawk_node:disconnect_or_delete_callback([
            {conn, fun() -> list_to_atom("ok") end}
        ])
    ),
    ?assertEqual(
        ok,
        hawk_node:disconnect_or_delete_callback([
            {disconn, fun() -> list_to_atom("ok") end}
        ])
    ).