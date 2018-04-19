-module(hawk_tests).
-include_lib("eunit/include/eunit.hrl").

% Not testing any of the hawk:call functionality
% it is covered by hawk_SUITE.erl

hawk_unit_test_() ->
    unit_testing:setup(
        % Setup
        fun() -> ok end,
        % Cleanup
        fun(_) -> ok end,
        % Tests
        [
            {"hawk:nodes/0",
                ?_assert(unit_testing:try_test_fun(fun nodes/0))},
            {"hawk:remove_node/1",
                ?_assert(unit_testing:try_test_fun(fun remove_node/0))}
        ],
        % Mocks started at setup phase
       [
            % Module
            {supervisor,
                % meck opts
                [unstick, passthrough],
                % expects
                [{which_children, fun
                    (hawk_nodes_sup) ->
                        [{hawk_node_node@rpmbp, self(), worker, [hawk_node]}];
                    (X) ->
                        supervisor:which_children(X)
                 end},
                 {terminate_child, fun
                    (hawk_nodes_sup, 'hawk_node_foo@bar') ->
                        ok;
                    (_, _) ->
                        {error,not_found}
                 end},
                 {delete_child, fun
                    (hawk_nodes_sup, 'hawk_node_foo@bar') ->
                        ok;
                    (_, _) ->
                        {error,not_found}
                 end}
                ]
            }
        ],
        % Strict meck unload
        true
    ).

nodes() ->
    ?assertEqual(
        [node@rpmbp],
        hawk:nodes()
    ).

remove_node() ->
    ?assertEqual(
        ok,
        hawk:remove_node('foo@bar')
    ),
    ?assertEqual(
        {error, no_such_node},
        hawk:remove_node('FAKE@bar')
    ).
