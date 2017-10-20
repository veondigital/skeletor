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
                {erlang_vsn, $e, "erlang-vsn", {string, "19.3"}, "specify the Erlang version to use"}
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
    Commands = [
        {{"Building image erlang/~s", [Name]},
         "docker build -t erlang/" ++ Name ++
                     " --build-arg ERLANG_VSN=" ++ VSN ++ " ."},
        {{"Exporting image ~s.tar", [Name]},
         "docker save -o " ++ Name ++ ".tar erlang/" ++ Name},
        {{"Compressing image ~s.tar.gz", [Name]},
         "gzip -9 " ++ Name ++ ".tar"},
        {{"Removing image erlang/~s", [Name]},
         "docker rmi erlang/" ++ Name}
    ],
    lists:foreach(fun run_cmd/1, Commands),
    rebar_api:info("Created ~s.tar.gz", [Name]),
    {ok, State}.

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
