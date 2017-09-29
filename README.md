Skeletor - Veon App Base
========================

This project is the base to create other Erlang projects. This file is a small guide to give you an idea of what is provided by the skeleton and what should be implemented.



HTTP Interface
--------------

The skeleton has a HTTP Interface for API created on top of [Elli](https://github.com/knutin/elli). The use is pretty simple. You can see it in the [veon_http module](src/veon_http.erl).

For the routing of the calls we can use the configuration as follow:

```erlang
{veon, [
    {http, [
        {veon_http, 8000}
    ]}
]}
```

It's possible only to create reating routes based on ports. The routing based on URI should be made inside of the module using pattern matching as suggested by the elli example in the web page:

```erlang
-module(veon_http).
-export([handle/2, handle_event/3]).

-behaviour(elli_handler).

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
```

This file could be found in [src/veon_http.erl](src/veon_http.erl).

Redis connection
----------------

The connection con Redis is granted using the configuration. For that you have to add there the following parameters:

```erlang
{veon, [
    {redis, [
        {host, "127.0.0.1"},
        {port, 6379},
        {database, 0},
        {password, ""},
        {connect_timeout, 5000},
        {reconnect_after, 100}
    ]}
]}
```

You could use Redis commands in whatever module:

```
{ok, <<"OK">>} = eredis:q(veon_redis, ["SET", "foo", "bar"]).
```

The registered name `veon_redis` is created from the [veon_redis](src/veon_redis.erl) wrapper to do the use of redis easier.

You can use this as well:

```
{ok, <<"OK">>} = veon_redis:q(["SET", "foo", "bar"]).
```

Logging
-------

The log facility is made via [lager](https://github.com/basho/lager). The parser transformation is configured in [rebar.config](rebar.config) so you can use the following functions:

```erlang
lager:debug(Format, Args).
lager:info(Format, Args).
lager:notice(Format, Args).
lager:warning(Format, Args).
lager:error(Format, Args).
lager:critical(Format, Args).
lager:alert(Format, Args).
lager:emergency(Format, Args).
```

And any other lager function you need. The configuration will be placed in the configuration file as the lager application.

Metrics
-------

We use the [prometheus](https://github.com/deadtrickster/prometheus.erl) library to gather information we want to store. The library in charge to expose that information to be collected from Prometheus is [prometheus-httpd](https://github.com/deadtrickster/prometheus-httpd).

The configuration in the *sys.config* file is as follow:

```erlang
{prometheus, [
    {prometheus_http, [
        {path, "/metrics"},
        {format, auto},
        {registry, auto},
        {telemetry_registry, default},
        {port, 8081},
        {authorization, false}
    ]}
]}.
```



XMPP Connection
---------------

We are going to use [snatch](https://github.com/xmppjingle/snatch) as base library to connect to the XMPP servers as a component. This library also let us to connect to the XMPP servers as a client so, it gives us enough flexibility.

You can see the implementation of the behaviour in the [src/veon_xmpp.erl](src/veon_xmpp.erl) file. And more information in the [README.md](https://github.com/xmppjingle/snatch/blob/master/README.md) file.

The configuration for *sys.config* is as follow:

```erlang
{veon, [
    {xmpp, [
        {host, "localhost"},
        {port, 8888},
        {domain, <<"comp.localhost">>},
        {password, <<"secret">>}
    ]}
]}
```


Configuration
-------------

We'll use the usual configuration for the most of the Erlang projects. The files `vm.args` and `sys.config` are located inside of the `config` directory.

Ensure you have the configuration correctly filled there.
