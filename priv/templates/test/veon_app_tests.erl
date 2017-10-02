-module({{name}}_app_tests).
-author('{{author_email}}').

-include_lib("eunit/include/eunit.hrl").

start_stop_test_() ->
    {timeout, 10000, fun() ->
        {ok, _} = application:ensure_all_started({{name}}),
        ok = application:stop({{name}})
    end}.
