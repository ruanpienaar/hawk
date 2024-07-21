-module(hawk_nodes_sup_tests).

-include_lib("eunit/include/eunit.hrl").

hawk_sup_unit_test_() ->
    unit_testing:setup(
        % Setup
        fun() -> ok end,
        % Cleanup
        fun(_) -> ok end,
        % Tests
        [
            {"hawk_nodes_sup:start_link/0",
                ?_assert(unit_testing:try_test_fun(fun start_link/0))},
            {"hawk_nodes_sup:init/1",
                ?_assert(unit_testing:try_test_fun(fun init/0))},
            {"hawk_nodes_sup:start_child/4",
                ?_assert(unit_testing:try_test_fun(fun start_child/0))},
            {"hawk_nodes_sup:delete_child/4",
                ?_assert(unit_testing:try_test_fun(fun delete_child/0))},
            {"hawk_nodes_sup:id/4",
                ?_assert(unit_testing:try_test_fun(fun id/0))}
        ],
        % Mocks started at setup phase
        [
            % Module
            {supervisor,
                % meck opts
                [unstick, passthrough],
                % expects
                [{start_link, fun({local, hawk_nodes_sup}, hawk_nodes_sup, {}) -> {ok, self()} end},
                 {start_child, fun
                    (
                        hawk_nodes_sup,
                        #{
                            id := 'hawk_node_foo@bar',
                            start := {hawk_node2, start_link, [foo@bar, cookie, [], []]},
                            restart := transient,
                            significant := false,
                            shutdown := 60000,
                            type := worker,
                            modules := [hawk_node2]
                        }
                    ) ->
                        {ok, self()};
                    (A, B) ->
                        ?debugFmt("~p ~p", [A, B]),
                        error
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

start_link() ->
    ?assertEqual(
        {ok, self()},
        hawk_nodes_sup:start_link()
    ).

init() ->
    ?assertEqual(
        {
            ok,
            {
                #{
                    restart => one_for_one,
                    intensity => 500,
                    period => 5
                },
                []
            }
        },
        hawk_nodes_sup:init({})
    ).

start_child() ->
    ?assertEqual(
        {ok, self()},
        hawk_nodes_sup:start_child(foo@bar, cookie, [], [])
    ).

% Running example in hawk_nodes_sup_SUITE
delete_child() ->
    % Not exist node
    ?assertEqual(
        {error, no_such_node},
        hawk_nodes_sup:delete_child('node@host.com')
    ),
    % Exists node
    ?assertEqual(
        ok,
        hawk_nodes_sup:delete_child('foo@bar')
    ).

id() ->
    ?assertEqual(
        'hawk_node_node@host.com',
        hawk_nodes_sup:id('node@host.com')
    ).