-module('{{name}}_redis').
-author('{{author_email}}').

-export([specs/1, start_link/1, q/1]).

-define(DEFAULT_MIN_WORKERS, 2).
-define(DEFAULT_MAX_WORKERS, 10).

-define(DEFAULT_HOST, "127.0.0.1").
-define(DEFAULT_PORT, 6379).
-define(DEFAULT_DB, 0).
-define(DEFAULT_PASS, "").
-define(DEFAULT_CONN_TIMEOUT, 5000).
-define(DEFAULT_RECONNECT_AFTER, 100).

start_link([Host, Port, DB, Pass, ConnTimeout, ReconnAfter]) ->
    eredis:start_link(Host, Port, DB, Pass, ConnTimeout, ReconnAfter).

specs([]) ->
    [];
specs(RedisCfg) ->
    PoolArgs = [{name, {local, ?MODULE}},
                {worker_module, ?MODULE},
                {size, min_workers(RedisCfg)},
                {max_overflow, max_workers(RedisCfg)}],
    [poolboy:child_spec(?MODULE, PoolArgs, get_cfg(RedisCfg))].

min_workers(Cfg) ->
    proplists:get_value(min_workers, Cfg, ?DEFAULT_MIN_WORKERS).

max_workers(Cfg) ->
    proplists:get_value(max_workers, Cfg, ?DEFAULT_MAX_WORKERS).

get_cfg(Cfg) ->
    [ proplists:get_value(host, Cfg, ?DEFAULT_HOST),
      proplists:get_value(port, Cfg, ?DEFAULT_PORT),
      proplists:get_value(database, Cfg, ?DEFAULT_DB),
      proplists:get_value(password, Cfg, ?DEFAULT_PASS),
      proplists:get_value(connect_timeout, Cfg, ?DEFAULT_CONN_TIMEOUT),
      proplists:get_value(reconnect_after, Cfg, ?DEFAULT_RECONNECT_AFTER) ].

q(Query) ->
    {_, RedisPoolSize, _Overflow, _Monitors} = poolboy:status(?MODULE),
    prometheus_gauge:set(redis_pool, RedisPoolSize),
    poolboy:transaction(?MODULE, fun(PID) ->
        eredis:q(PID, Query)
    end).
