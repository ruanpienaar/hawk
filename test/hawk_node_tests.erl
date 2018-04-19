-module(hawk_node_tests).
-include_lib("eunit/include/eunit.hrl").

% Not testing any of the process code
% it is covered by hawk_SUITE.erl

hawk_node_unit_test_() ->
    {setup,
     % Setup Fixture
     fun() ->
         xxx
     end,
     % Cleanup Fixture
     fun(xxx) ->
         ok
     end,
     % List of tests
     [
       % Example test
       {"hawk_node:initial_state/4",
            ?_assert(unit_testing:try_test_fun(fun initial_state/0))}

        % initial_state(Node, Cookie, ConnectedCallbacks, DisconnectedCallbacks)
        % connected_callback(CCBL) ->
        % disconnect_or_delete_callback(DCBL) ->
     ]
    }.

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
