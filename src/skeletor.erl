-module(skeletor).

-export([init/1, run_cmd/1]).

-define(TIME_TO_FINISH, 300000). % 5 minutes

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Modules = [
        skeletor_docker_build,
        skeletor_docker_release,
        skeletor_docker_push
    ],
    {ok, lists:foldl(fun(Module, S) ->
        {ok, State1} = Module:init(S),
        State1
    end, State, Modules)}.

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
