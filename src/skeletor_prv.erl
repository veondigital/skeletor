-module(skeletor_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, docker).
-define(DEPS, [app_discovery]).

-define(TIME_TO_FINISH, 300000). % 5 minutes

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 docker"}, % How to use the plugin
            {opts, [
                % list of options understood by the plugin
                {only_build, $b, "only-build", integer, "only builds (compile) the artifact"},
                {only_package, $p, "only-package", integer, "only package (release) the artifcat"},
                {erlang_vsn, $e, "erlang-vsn", {string, "19.3"}, "specify the Erlang version to use"},
                {no_delete_img, $k, "keep-img", integer, "no remove image"},
                {no_compress_img, $C, "no-gzip", integer, "no compress (gzip -9) the exported image"}
            ]},
            {short_desc, "Create a docker image with the release inside"},
            {desc, "Create a docker image with the release inside. It uses Debian Stretch and Erlang 19.3 as default."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    VSN = proplists:get_value(erlang_vsn, Args),
    rebar_api:info("Erlang version ~s", [VSN]),
    Name = filename:basename(rebar_state:dir(State)),
    case is_only_build(Args) of
        true ->
            build(Name, Args, VSN),
            case is_only_package(Args) of
                true ->
                    package(Name, Args),
                    final_msg(package, Args, Name);
                false ->
                    final_msg(build, Args, Name)
            end;
        false ->
            case is_only_package(Args) of
                true -> package(Name, Args);
                false ->
                    build(Name, Args, VSN),
                    package(Name, Args)
            end,
            final_msg(package, Args, Name)
    end,
    {ok, State}.

final_msg(package, Args, Name) ->
    case is_compress(Args) of
        true -> rebar_api:info("Created ~s.tar.gz", [Name]);
        false -> rebar_api:info("Created ~s.tar", [Name])
    end;
final_msg(build, _Args, Name) ->
    rebar_api:info("Compiled ~s", [Name]).

build(Name, Args, VSN) ->
    Commands = [
        {{"Building image erlang/build-~s", [Name]},
         "docker build -t erlang/build-" ++ Name ++
                     " --build-arg ERLANG_VSN=" ++ VSN ++
                     " -f Dockerfile.build " ++
                     " ."},
        {{"Running builder erlang/build-~s", [Name]},
         "docker run --rm -v `pwd`:/opt/veon -i "
         "erlang/build-" ++ Name}
    ] ++ maybe_remove(build, Args, Name),
    lists:foreach(fun run_cmd/1, Commands),
    ok.

package(Name, Args) ->
    Commands = [
        {{"Building final image erlang/~s", [Name]},
         "docker build -t erlang/" ++ Name ++
                     " -f Dockerfile.release " ++
                     " ."},
        {{"Exporting image ~s.tar", [Name]},
         "docker save -o " ++ Name ++ ".tar erlang/" ++ Name}
    ] ++ maybe_compress(Args, Name)
      ++ maybe_remove(package, Args, Name),
    lists:foreach(fun run_cmd/1, Commands),
    ok.

is_only_package(Args) ->
    proplists:get_value(only_package, Args) =/= undefined.

is_only_build(Args) ->
    proplists:get_value(only_build, Args) =/= undefined.

is_remove(Args) ->
    proplists:get_value(no_delete_img, Args) =:= undefined.

maybe_remove(build, Args, Name) ->
    case is_remove(Args) of
        true ->
            [{{"Removing image erlang/build-~s", [Name]},
              "docker rmi -f erlang/build-" ++ Name}];
        false ->
            []
    end;
maybe_remove(package, Args, Name) ->
    case is_remove(Args) of
        true ->
            [{{"Removing image erlang/~s", [Name]},
              "docker rmi erlang/" ++ Name}];
        false ->
            []
    end.

is_compress(Args) ->
    proplists:get_value(no_compress_img, Args) =:= undefined.

maybe_compress(Args, Name) ->
    case is_compress(Args) of
        true ->
            [{{"Compressing image ~s.tar.gz", [Name]},
              "gzip -9f " ++ Name ++ ".tar"}];
        false ->
            []
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

run_cmd({{Fmt, Args}, Command}) ->
    rebar_api:info(Fmt, Args),
    Opts = [exit_status, stderr_to_stdout],
    rebar_api:debug("command: ~s", [Command]),
    Port = open_port({spawn, Command}, Opts),
    process_output(Port).

process_output(Port) ->
    receive
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, _}} ->
            rebar_api:abort("docker returns error!", []);
        {Port, {data, Data}} ->
            rebar_api:debug("docker output: ~s", [Data]),
            process_output(Port)
    after ?TIME_TO_FINISH ->
        rebar_api:abort("too much time waiting for finalizing!", [])
    end.
