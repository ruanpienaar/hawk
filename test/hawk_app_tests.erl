-module(hawk_app_tests).
-include_lib("eunit/include/eunit.hrl").

hawk_app_unit_test_() ->
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
       {"hawk_app:func1/0",
            ?_assert(unit_testing:try_test_fun(fun func1/0))}
     ]
    }.

func1() ->
    ?assert(
        is_list(hawk_app:module_info())
    ).
