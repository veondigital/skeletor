-module(veon_app).
-author('manuel.rubio@veon.com').

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    {ok, {
        #{strategy => one_for_one,
          intensity => 10,
          period => 1},
        children()
    }}.

children() ->
    veon_redis:specs(application:get_env(veon, redis, [])) ++
    veon_http:specs(application:get_env(veon, http, [])) ++
    veon_xmpp:specs(application:get_env(veon, xmpp, [])) ++
    veon_metrics:specs(application:get_env(veon, metrics, [])).
