-module('{{name}}_app').
-author('{{author_email}}').

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).
-export([to_int/1, to_str/1, to_bin/1]).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    lists:foreach(fun({Module, _ConfigKey}) ->
        case lists:member({unload, 0}, Module:module_info(exports)) of
            true -> Module:unload();
            false -> ok
        end
    end, children_data()),
    %% ensure prometheus_http is cleaning its interfaces:
    lists:foreach(fun({httpd, PID, _}) -> inets:stop(httpd, PID);
                     (_) -> ok
                  end, inets:services_info()),
    ok.

init([]) ->
    {ok, {
        #{strategy => one_for_one,
          intensity => 10,
          period => 1},
        children()
    }}.

children_data() ->
    [
        {'{{name}}_cache', redis},
        {'{{name}}_http', http},
        {'{{name}}_snatch', snatch},
        {'{{name}}_metrics', metrics}
    ].

children() ->
    lists:flatmap(fun({Module, ConfigKey}) ->
        ConfigVal = application:get_env('{{name}}', ConfigKey, []),
        case lists:member({load, 0}, Module:module_info(exports)) of
            true -> Module:load();
            false -> ok
        end,
        Module:specs(ConfigVal)
    end, children_data()).

to_int(Num) when is_number(Num) -> Num;
to_int(Str) when is_list(Str) -> list_to_integer(Str);
to_int(Bin) when is_binary(Bin) -> binary_to_integer(Bin).

to_str(Num) when is_integer(Num) -> integer_to_list(Num);
to_str(Str) when is_list(Str) -> Str;
to_str(Bin) when is_binary(Bin) -> binary_to_list(Bin).

to_bin(Num) when is_integer(Num) -> integer_to_binary(Num);
to_bin(Str) when is_list(Str) -> list_to_binary(Str);
to_bin(Bin) when is_binary(Bin) -> Bin;
to_bin(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8).
