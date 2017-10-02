-module({{name}}_redis).
-author('{{author_email}}').

-export([specs/1, q/1]).

specs([]) ->
    [];
specs(RedisCfg) ->
    [ #{ id => ?MODULE,
         start => {eredis, start_link, get_cfg(RedisCfg)},
         restart => permanent,
         shutdown => 5000,
         type => worker,
         modules => [eredis]} ].

get_cfg(Cfg) ->
    [ proplists:get_value(host, Cfg, undefined),
      proplists:get_value(port, Cfg, 6379),
      proplists:get_value(database, Cfg, 0),
      proplists:get_value(password, Cfg, ""),
      proplists:get_value(connect_timeout, Cfg, 5000),
      proplists:get_value(reconnect_after, Cfg, 100) ].

q(Query) ->
    eredis:q(?MODULE, Query).
