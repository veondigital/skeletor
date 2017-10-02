-module(veon_metrics).
-author('manuel.rubio@veon.com').

-export([start_link/0, specs/1]).

start_link() ->
    application:start(prometheus),
    application:ensure_all_started(prometheus_httpd),
    {ok, _PID} = prometheus_httpd:start().

specs([]) ->
    [ #{ id => veon_metrics,
         start => {?MODULE, start_link, []},
         restart => permanent,
         shutdown => 5000,
         type => worker,
         modules => [prometheus_httpd]} ].
