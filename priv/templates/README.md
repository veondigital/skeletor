{{name}}
========================

{{desc}}

Kafka consumer
--------------

We use snatch and by default we are integrating the possibility to configure a Snatch configuration to receive and even send events from/to Kafka.

The configuration inside of `{{name}}` configuration and inside of `snatch` subsection is as follow:

```erlang
{kafka, [
    {endpoints, [{"localhost", 9092}]},
    {in_topics, [{<<"{{name}}.in">>, [0]}]},
    {raw, true}
]},
```

This way we can configure several incoming events from the topic configured for the endpoints connected to.

Kafka producer
--------------

In the same way as the previous we can setup a topic to do the send of the events via Kafka:

```erlang
{kafka, [
    {endpoints, [{"localhost", 9092}]},
    {in_topics, [{<<"{{name}}.in">>, [0]}]},
    {out_topic, <<"{{name}}.out">>},
    {out_partition, 0},
    {raw, true}
]},
```

For output (produce) we can only configure one topic and one partition. This way we avoid ambiguity.

REST
----

This is intended to be a response processor for the claw. The way to work with this is:

1. Client request to the client something (using `claws_rest:request`).
2. Call was async so when the server responds the response arrives to the claw and it's moved to snatch (`snatch:received`).
3. Snatch send the event to the implementation or listener (handler).

The configuration of REST avoid to use the URL inside of the code. This way we work always only with URI.

Note that the REST claw is designed to be called always for the same domain with persistent (keeping alive) connections to the web server.

The configuration for the `{{name}}` section and `snatch` subsection is as follow:

```erlang
{rest, [
    {domain, "localhost"},
    {port, 80},
    {schema, "http"},
    {max_sessions, 10},
    {max_pipeline_size, 1}
]}
```

See [ibrowse](https://github.com/cmullaparthi/ibrowse/blob/master/README.md) documentation for further information.

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

Docker image
------------

The system is prepared to build an image with the base code of the component inside. You only needs to run:

```
./rebar3 as prod do docker
```

This creates the image `erlang/{{name}}` and when is created it's exported to the `{{name}}.tar.gz` file and then the image is removed.

Note that to works with this you have to install Docker first. You can do this easily in MacOS and GNU/Linux. Check the Docker website for further information.

The way it's working is creating two images actually. The first one is called `erlang/build-{{name}}` and the second one is `erlang/{{name}}`. The first one is intended to compile and build the release. The second one is only to contain the final artifact and export the image containing everything needed to run the system.

The options you can use with this command (`docker`) are the following:

- `--only-build` (or `-b`) let you to do only the first step, compile and generate the release. It uses and creates only the first image (`erlang/build-{{name}}`), run it to create the artifact and then remove it.
- `--only-package` (or `-p`) let you to do only the release/package step. It is on charge of create the second image (`erlang/{{name}}`) and put the artifact inside. It also create the tarball file and compress it.
- `--erlang-vsn` (or `-e`) let you to decide which version of Erlang/OTP you want to use to build and create the release. The Erlang/OTP system is embeded inside of the artifact so you can use whatever version that fits well with your code. According to snatch, elli and other dependencies, you should to choose the 19.3 as recommended or greater.
- `--keep-img` (or `-k`) let you to keep the images generated. Both `erlang/build-{{name}}` and `erlang/{{name}}` are kept if you use this option.
- `--no-gzip` (or `-C`) avoid to generate the compressed version of the artifact. By default the system creates the file `{{name}}.tar`. There are another step to create the file `{{name}}.tar.gz` but if you use this option this last step is not performed.

For example, if we want to build the artifact (compilation only) and avoid to remove the building image:

```
./rebar3 as prod do docker -b -k
```

Or using the extended arguments:

```
./rebar3 as prod do docker --only-build --keep-img
```

This could help to compile and recompile faster if you keep the image. This way the previous steps are not performed (there are performed previously) and only the compilation is performed.

This way you can perform after that the packaging in this way:

```
./rebar3 as prod do docker --only-package --no-gzip
```

Note that the final artifact is only around 200MB. If you perform the compression step (takes almost a minute) and it could compress until 80MB or a bit less. It's your choice compress or not.

To perform a CI generation you could do it in this way:

```
./rebar3 as prod do docker --erlang-vsn 19.3 --no-gzip
```

This way you keep the images, you are selection explicitly the Erlang/OTP version and avoiding the compression step. Sometimes in the CI systems it's better to perform all of the steps to avoid weird behaviours derivated to previous builds, that's the reason because I'm not keeping the images in this command.

Running a release
-----------------

A release is packaged with all of you need to run the system: binaries, erts (including BEAM), configuration and scripts.

The configuration, as is, isn't complete. We have a set of environment variables that should be defined to work properly. I'm going to add here all of them. Ideally you have to set them before (or in the same line) you run the `{{name}}` command.

- `NODE_NAME` (vm.args) this variable set the name to be used. You can set only the node part of the node name or the whole one. Note that the name is configured using `-name` so you should to provide a FQDN if you want to configure the whole node name (default: {{name}}).
- `NODE_COOKIE` (vm.args) contains the cookie used to connect to other Erlang nodes (default: {{name}}_cookie).
- `HTTP_PORT` (sys.config) the HTTP interface included in the application is starting using a port. This must to be defined using this environment variable to avoid to fail (default: 8000).
- `REDIS_HOST` (sys.config) the Redis IP or hostname which to connect (default: 127.0.0.1).
- `REDIS_PORT` (sys.config) the Redis port which to connect (default: 6379).
- `REDIS_DB` (sys.config) the Redis database (default: 0).
- `REDIS_PASS` (sys.config) the Redis password to access to the database. Usually Redis hasn't password configured (default is empty: "").
- `XMPP_HOST` (sys.config) XMPP Server which to connect (default: 127.0.0.1).
- `XMPP_PORT` (sys.config) XMPP Server port which to connect as a component (default: 8888).
- `XMPP_DOMAIN` (sys.config) XMPP Domain which will be in use to the XMPP connection (default: comp.localhost).
- `XMPP_SECRET` (sys.config) XMPP Secret to use in the component connection for the XMPP Server (default: secret).

If you want to change some or all of these configuration data, you only need to provide them as environment variables.

Those variables are defined inside of the `start-prod.sh` script available in the *bin* directory when you create a release (and in the `script` directory as source base).

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
