-module({{name}}_metrics).
-author('{{author_email}}').

-export([specs/1]).

specs([]) ->
    [ #{ id => ?MODULE,
         start => {prometheus_httpd, start, []},
         restart => permanent,
         shutdown => 5000,
         type => worker,
         modules => [prometheus_httpd]} ].
