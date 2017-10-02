-module(veon_app_tests).
-author('manuel.rubio@veon.com').

-include_lib("eunit/include/eunit.hrl").

start_stop_test_() ->
    {timeout, 10000, fun() ->
        {ok, [veon]} = application:ensure_all_started(veon),
        ok = application:stop(veon)
    end}.
