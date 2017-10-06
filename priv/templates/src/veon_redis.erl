-module('{{name}}_redis').
-author('{{author_email}}').

-export([specs/1, q/1]).

-define(DEFAULT_MIN_WORKERS, 2).
-define(DEFAULT_MAX_WORKERS, 10).

specs([]) ->
    [];
specs(RedisCfg) ->
    PoolArgs = [{name, {local, ?MODULE}},
                {worker_module, eredis},
                {size, min_workers(RedisCfg)},
                {max_overflow, max_workers(RedisCfg)}],
    [poolboy:child_spec(?MODULE, PoolArgs, get_cfg(RedisCfg))].

min_workers(Cfg) ->
    proplists:get_value(min_workers, Cfg, ?DEFAULT_MIN_WORKERS).

max_workers(Cfg) ->
    proplists:get_value(max_workers, Cfg, ?DEFAULT_MAX_WORKERS).

get_cfg(Cfg) ->
    [ proplists:get_value(host, Cfg, undefined),
      proplists:get_value(port, Cfg, 6379),
      proplists:get_value(database, Cfg, 0),
      proplists:get_value(password, Cfg, ""),
      proplists:get_value(connect_timeout, Cfg, 5000),
      proplists:get_value(reconnect_after, Cfg, 100) ].

q(Query) ->
    {_, RedisPoolSize, _Overflow, _Monitors} = poolboy:status(?MODULE),
    prometheus_gauge:set(redis_pool, RedisPoolSize),
    poolboy:transaction(?MODULE, fun(PID) ->
        eredis:q(PID, Query)
    end).
