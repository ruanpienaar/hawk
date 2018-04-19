-module(hawk_sup_tests).

-include_lib("eunit/include/eunit.hrl").

hawk_sup_unit_test_() ->
    unit_testing:setup(
        % Setup
        fun() ->
            % dbg:tracer(),
            % dbg:p(all ,call),
            % dbg:tpl(hawk_sup, cx),
            ok
        end,
        % Cleanup
        fun(_) -> ok end,
        % Tests
        [
            {"hawk_sup:start_link/0",
                ?_assert(unit_testing:try_test_fun(fun start_link/0))},
            {"hawk_sup:init/1",
                ?_assert(unit_testing:try_test_fun(fun init/0))}
        ],
        % Mocks started at setup phase
        [
            % Module
            {supervisor,
                % meck opts
                [unstick, passthrough],
                % expects
                [{start_link, fun({local, hawk_sup}, hawk_sup, []) -> {ok, self()} end}
                ]
            }
        ],
        % Strict meck unload
        true
    ).

start_link() ->
    ?assertEqual(
        {ok, self()},
        hawk_sup:start_link()
    ).

init() ->
    ?assertEqual(
        {ok, { {one_for_one, 500, 10}, [
            {hawk_nodes_sup,
                {hawk_nodes_sup, start_link, []}, permanent, 5000, supervisor,
                [hawk_nodes_sup]},
            {hawk_node_mon_sup,
                {hawk_node_mon_sup, start_link, []}, permanent, 5000, supervisor,
                [hawk_node_mon_sup]}
        ]} },
        hawk_sup:init([])
    ).