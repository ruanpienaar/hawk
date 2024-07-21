-module(hawk_app_tests).
-include_lib("eunit/include/eunit.hrl").

hawk_app_unit_test_() ->
    unit_testing:setup(
        % Setup
        fun() -> ok end,
        % Cleanup
        fun(_) -> ok end,
        % Tests
        [
            {"hawk_app:start_link/0",
                ?_assert(unit_testing:try_test_fun(fun start_link/0))},
            {"hawk_app:stop/1",
                ?_assert(unit_testing:try_test_fun(fun stop/0))}
        ],
        % Mocks started at setup phase
       [
            % Module
            {supervisor,
                % meck opts
                [unstick, passthrough],
                % expects
                [{start_link, fun(hawk_sup, {}) -> {ok, self()} end}
                ]
            }
        ],
        % Strict meck unload
        true
    ).

start_link() ->
    ?assertEqual(
        {ok, self()},
        hawk_app:start(normal, normal)
    ).

stop() ->
    ?assertEqual(
        ok,
        hawk_app:stop(state)
    ).