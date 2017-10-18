-module('{{name}}_app_tests').
-author('{{author_email}}').

-include_lib("eunit/include/eunit.hrl").

start_stop_test_() ->
    {timeout, 10000, fun() ->
        application:load(prometheus),
        Metrics = [
            {histogram, [{name, users_session_duration_seconds},
                         {labels, [usertype]},
                         {buckets, [30, 60, 300, 600, 900, 1800, 3600]},
                         {help, "User session duration in secs"}]},
            {gauge, [{name, redis_pool},
                     {help, "Size of the redis pool requirements"}]},
            {counter, [{name, stanza_counter},
                       {help, "The stanza number of elements handled"}]},
            {summary, [{name, xmpp_kb_sent},
                       {help, "Size of the stanzas sent to XMPP server"}]}
        ],
        application:set_env(prometheus, default_metrics, Metrics),
        {ok, _} = application:ensure_all_started('{{name}}'),
        ok = application:stop('{{name}}')
    end}.
