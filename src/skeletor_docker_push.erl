-module(skeletor_docker_push).

-export([init/1, do/1, format_error/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, push},
            {module, ?MODULE},
            {bare, true},
            {deps, [{default, compile}]},
            {example, "rebar3 docker push"},
            {opts, [
                {tag, $t, "tag", {string, undefined}, "tag to push the image"}
            ]},
            {short_desc, "Push docker image to the registry"},
            {desc, "Push docker image to the registry. Check configuration in rebar.config for further information."},
            {namespace, docker}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    App = hd(rebar_state:project_apps(State)),
    Name = binary_to_list(rebar_app_info:name(App)),
    VSN = rebar_app_info:original_vsn(App),
    publish(tag(State), Name, VSN),
    rebar_api:info("Pushed ~s (~s)", [Name, VSN]),
    {ok, State}.

tag(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(tag, Args) of
        undefined ->
            Docker = rebar_state:get(State, docker, []),
            case proplists:get_value(tag, Docker) of
                undefined ->
                    rebar_api:abort("tag isn't configured!", []);
                "" ->
                    rebar_api:abort("tag isn't configured!", []);
                Tag ->
                    Tag
            end;
        Tag ->
            Tag
    end.

publish(Tag, Name, VSN) ->
    ImageName = "erlang/" ++ Name,
    PushName = Tag ++ ":" ++ VSN,
    Commands = [
        {{"Tagging image ~s", [PushName]},
         "docker tag " ++ ImageName ++ " " ++ PushName},
        {{"Pushing image ~s", [PushName]},
         "docker push " ++ PushName}],
    lists:foreach(fun skeletor:run_cmd/1, Commands),
    ok.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
