-module(test_common).

-export([config_prometheus/0,
         start_prometheus/0,
         stop_prometheus/0,
         start_cache/0,
         stop_cache/1]).

start_cache() ->
    PoolArgs = [{name, {local, '{{name}}_cache'}},
                {worker_module, '{{name}}_cache'},
                {size, 1},
                {max_overflow, 1}],
    RedisArgs = ["127.0.0.1", 6379, 0, "", 5000, 100],
    {ok, _PoolPID} = poolboy:start_link(PoolArgs, RedisArgs).

stop_cache(PID) ->
    ok = poolboy:stop(PID),
    ok.

config_prometheus() ->
    application:load(prometheus),
    Metrics = [
        {gauge, [{name, redis_pool},
                 {help, "Size of the redis pool requirements"}]},
        {counter, [{name, cache_get}, {help, "GET commands"}]},
        {counter, [{name, cache_get_hit},
                   {help, "GET commands hit in the cache"}]},
        {counter, [{name, cache_get_miss},
                   {help, "GET commands miss in the cache"}]},
        {counter, [{name, cache_set}, {help, "SET commands"}]},
        {counter, [{name, cache_set_temporal},
                   {help, "EXPIRE commands"}]},
        {counter, [{name, cache_set_persistent},
                   {help, "PERSIST commands"}]},
        {counter, [{name, stanza_counter},
                   {help, "The stanza number of elements handled"}]},
        {summary, [{name, xmpp_kb_sent},
                   {help, "Size of the stanzas sent to XMPP server"}]}
    ],
    application:set_env(prometheus, default_metrics, Metrics),
    application:unset_env(prometheus, prometheus_http),
    ok.

start_prometheus() ->
    ok = config_prometheus(),
    ok = application:start(prometheus),
    ok.

stop_prometheus() ->
    ok = application:stop(prometheus),
    ok = application:unload(prometheus),
    ok.
