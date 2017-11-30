-module(skeletor_docker_release).

-export([init/1, do/1, format_error/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, release},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, app_discovery}]},
            {example, "rebar3 docker-release"},
            {opts, []},
            {short_desc, "Create a docker image with the release inside"},
            {desc, "Create a docker image with the release inside. It uses Debian Stretch."},
            {namespace, docker}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Apps = rebar_state:project_apps(State),
    Name = binary_to_list(rebar_app_info:name(hd(Apps))),
    package(Name, Args),
    rebar_api:info("Created image erlang/~s", [Name]),
    {ok, State}.

package(Name, _Args) ->
    Commands = [
        {{"Building final image erlang/~s", [Name]},
         "docker build -t erlang/" ++ Name ++
                     " -f Dockerfile.release " ++
                     " ."}
    ],
    lists:foreach(fun skeletor:run_cmd/1, Commands),
    ok.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
