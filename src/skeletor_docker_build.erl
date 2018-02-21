-module(skeletor_docker_build).

-export([init/1, do/1, format_error/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, build},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, app_discovery}]},
            {example, "rebar3 docker-build"},
            {opts, [
                % list of options understood by the plugin
                {no_delete_img, $k, "keep-img", integer, "no remove image"}
            ]},
            {short_desc, "Create a docker image to build the project"},
            {desc, "Create a docker image to build the project."},
            {namespace, docker}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Apps = rebar_state:project_apps(State),
    Name = binary_to_list(rebar_app_info:name(hd(Apps))),
    build(Name, Args),
    rebar_api:info("Compiled ~s", [Name]),
    {ok, State}.

build(Name, Args) ->
    Commands = [
        {{"Building image erlang/build-~s", [Name]},
         "docker build -t erlang/build-" ++ Name ++
                     " -f Dockerfile.build " ++
                     " ."},
        {{"Running builder erlang/build-~s", [Name]},
         "docker run --rm -v `pwd`:/opt/" ++ Name ++ " -i "
         "erlang/build-" ++ Name}
    ] ++ maybe_remove(Args, Name),
    lists:foreach(fun skeletor:run_cmd/1, Commands),
    ok.

is_remove(Args) ->
    proplists:get_value(no_delete_img, Args) =:= undefined.

maybe_remove(Args, Name) ->
    case is_remove(Args) of
        true ->
            [{{"Removing image erlang/build-~s", [Name]},
              "docker rmi -f erlang/build-" ++ Name}];
        false ->
            []
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
