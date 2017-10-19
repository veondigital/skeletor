-module('{{name}}_http').
-author('{{author_email}}').

-export([specs/1, handle/2, handle_event/3]).

-behaviour(elli_handler).

specs(ElliOps) ->
    [ #{ id => get_id(ElliOp),
         start => {elli, start_link, [get_cfg(ElliOp)]},
         restart => permanent,
         shutdown => 5000,
         type => worker,
         modules => [elli]} || ElliOp <- ElliOps ].

get_id({_Module, Port}) when is_integer(Port) ->
    list_to_atom("http_interface_port_" ++ integer_to_list(Port)).

get_cfg({Module, Port}) when is_atom(Module) andalso is_integer(Port) ->
    [{callback, Module}, {port, Port}].

handle(Req, _Args) ->
    %% Delegate to our handler function
    handle(elli_request:method(Req), elli_request:path(Req), Req).

handle('GET',[<<"hello">>, <<"world">>], _Req) ->
    %% Reply with a normal response. 'ok' can be used instead of '200'
    %% to signal success.
    {ok, [], <<"Hello World!">>};

handle(_, _, _Req) ->
    {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
    ok.
