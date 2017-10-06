{{name}}
========================

{{desc}}

HTTP Interface
--------------

The skeleton has a HTTP Interface for API created on top of [Elli](https://github.com/knutin/elli). The use is pretty simple. You can see it in the [{{name}}_http module](src/{{name}}_http.erl).

For the routing of the calls we can use the configuration as follow:

```erlang
{'{{name}}', [
    {http, [
        {'{{name}}_http', 8000}
    ]}
]}
```

It's possible only to create reating routes based on ports. The routing based on URI should be made inside of the module using pattern matching as suggested by the elli example in the web page:

```erlang
-module('{{name}}_http').
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

This file could be found in [src/{{name}}_http.erl](src/{{name}}_http.erl).

Redis connection
----------------

The connection con Redis is granted using the configuration. For that you have to add there the following parameters:

```erlang
{'{{name}}', [
    {redis, [
        {host, "127.0.0.1"},
        {port, 6379},
        {database, 0},
        {password, ""},
        {connect_timeout, 5000},
        {reconnect_after, 100},
        {min_workers, 2},
        {max_workers, 10}
    ]}
]}
```

Redis is running under `poolboy` so, you can use the simplification `{{name}}_redis:q/1` as follow:

```
{ok, <<"OK">>} = {{name}}_redis:q(["SET", "foo", "bar"]).
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
    ]},
    {default_metrics, [
        {histogram, [{name, users_online},
                     {labels, [method]},
                     {buckets, [100, 300, 500, 750, 1000]},
                     {help, "Users online"}]}
    ]}
]}.
```

Further information about the default metrics [here](https://github.com/deadtrickster/prometheus.erl#example-console-session).


XMPP Connection
---------------

We are going to use [snatch](https://github.com/xmppjingle/snatch) as base library to connect to the XMPP servers as a component. This library also let us to connect to the XMPP servers as a client so, it gives us enough flexibility.

You can see the implementation of the behaviour in the [src/{{name}}_xmpp.erl](src/{{name}}_xmpp.erl) file. And more information in the [README.md](https://github.com/xmppjingle/snatch/blob/master/README.md) file.

The configuration for *sys.config* is as follow:

```erlang
{'{{name}}', [
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

Working with the Project
========================

Compiling and cleaning
----------------------

The way to compile everything is:

```
./rebar3 compile
```

This is in charge of download all of the needed dependencies and compile all of them. If you need to force the compilation you can perform first a clean:

```
./rebar3 clean
```

However, that's only cleaning the local code, the dependencies remain as are. To force the complete build of the system, you can remove the `_build` directory:

```
rm -rf _build
```

And then `compile` again.

Testing
-------

The tests are placed in the `test` directory. You can create the tests you want against the code you created (or exists in the skeletor base). Usually the name of the files are corresponding one of the file in the `src` directory. That's because you can find `{{name}}_app_tests` to test `{{name}}_app` module.

To create new ones ensure you follow that nomenclature to do an easy follow of the code and tests.

To run the tests you only need to execute:

```
./rebar3 do xref, eunit, cover
```

These are three differente commands, but they work together to ensure your code is consistent. These commands are in charge of:

- `xref` check if the functions you are using exist actually.
- `eunit` run the EUnit tests you wrote under `test` directory.
- `cover` generates the coverage output. HTML files placed in the directory `_build/test/cover` directory and a brief of all of the files in screen at the execution moment.

Release
-------

To generate a new release you can execute:

```
./rebar3 as prod release

```

This command generates a release under `_build/prod/rel/{{name}}` directory. The system could be run or executed using the command:

```
_build/prod/rel/{{name}}/bin/{{name}} console
```

That will start a foreground execution of the system with a shell where you can type commands. If you interrupt (Ctrl+C) the system, it'll finnish.

To generate a tarball file you can run:

```
./rebar3 as prod tar
```

This will generate a tarball file in `_build/prod/rel/{{name}}/{{name}}-0.1.0.tar.gz` (note that the version depends on the version you used in the `{{name}}.app.src` file).

Release upgrades
----------------

To use the capability of non-stop you can create a `relup`. This is a new release from an older one (for example 0.2.0). You have to keep the previous one (at least the tarball file) and then instead of run `release`or `tar` you have to run:

```
./rebar3 as prod relup
```

Then a new release will be generated based on the previous one. This means you can use the `_build/prod/rel/{{name}}/bin/{{name}} upgrade ...` command.

Troubleshooting
---------------

If you have some issues, you can create an issue in this project to do a following of that and solve it as soon as possible. In the same way, you can find in the list of issues just in case your issue happened to anyone else before and you can find there the solution.

Enjoy!
