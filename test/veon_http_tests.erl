-module(veon_http_tests).
-author('manuel.rubio@veon.com').

-include_lib("eunit/include/eunit.hrl").

-export([init/1]).

init([]) ->
    {ok, {
        #{strategy => one_for_one,
          intensity => 1,
          period => 1},
        veon_http:specs([{veon_http, 8000}])
    }}.

simple_request_test_() ->
    {timeout, 10000, fun() ->
        {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
        true = unlink(Sup),
        ?assertEqual({200, "Hello World!"},
                     request("http://localhost:8000/hello/world")),
        ?assertEqual({404, "Not Found"},
                     request("http://localhost:8000/not-found")),
        true = exit(Sup, shutdown)
    end}.

request(URL) ->
    {ok, {{_, Code, _}, _Headers, Content}} =
        httpc:request(get, {URL, []}, [], []),
    {Code, Content}.
