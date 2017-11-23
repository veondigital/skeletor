-module('{{name}}_app').
-author('{{author_email}}').

-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1, init/1]).
-export([to_int/1, to_str/1, to_bin/1]).

start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    {ok, {
        #{strategy => one_for_one,
          intensity => 10,
          period => 1},
        children()
    }}.

children() ->
    '{{name}}_redis':specs(application:get_env('{{name}}', redis, [])) ++
    '{{name}}_http':specs(application:get_env('{{name}}', http, [])) ++
    '{{name}}_snatch':specs(application:get_env('{{name}}', snatch, [])) ++
    '{{name}}_metrics':specs(application:get_env('{{name}}', metrics, [])).

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
