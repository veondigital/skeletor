-module({{name}}_app).
-author('{{author_email}}').

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
    {{name}}_redis:specs(application:get_env({{name}}, redis, [])) ++
    {{name}}_http:specs(application:get_env({{name}}, http, [])) ++
    {{name}}_xmpp:specs(application:get_env({{name}}, xmpp, [])) ++
    {{name}}_metrics:specs(application:get_env({{name}}, metrics, [])).
