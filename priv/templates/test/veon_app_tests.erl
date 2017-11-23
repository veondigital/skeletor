-module('{{name}}_app_tests').
-author('{{author_email}}').

-include_lib("eunit/include/eunit.hrl").

-import(test_common, [config_prometheus/0, stop_prometheus/0]).

start_stop_test_() ->
    {timeout, 10000, fun() ->
        config_prometheus(),
        {ok, _} = application:ensure_all_started('{{name}}'),
        ok = application:stop('{{name}}'),
        ok = stop_prometheus()
    end}.
