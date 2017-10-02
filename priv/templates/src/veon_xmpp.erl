-module('{{name}}_xmpp').
-author('{{author_email}}').

-behaviour(snatch).

-include_lib("snatch/include/snatch.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-export([specs/1, init/1, handle_info/2, terminate/2, send/1,
         start_link/1]).

-record(state, {}).

-define(DEFAULT_HOST, undefined).
-define(DEFAULT_PORT, 8888).
-define(DEFAULT_DOMAIN, undefined).
-define(DEFAULT_PASS, <<"secret">>).

specs(Opts) ->
    SnatchArgs = [claws_xmpp_comp, ?MODULE, []],
    ClawArgs = [#{
        host => proplists:get_value(host, Opts, ?DEFAULT_HOST),
        port => proplists:get_value(port, Opts, ?DEFAULT_PORT),
        domain => proplists:get_value(domain, Opts, ?DEFAULT_DOMAIN),
        password => proplists:get_value(password, Opts, ?DEFAULT_PASS)
    }],
    [ #{ id => '{{name}}_xmpp_snatch',
         start => {snatch, start_link, SnatchArgs},
         restart => permanent,
         shutdown => 5000,
         type => worker,
         modules => [snatch]},
      #{ id => '{{name}}_xmpp_claw',
         start => {?MODULE, start_link, ClawArgs},
         restart => permanent,
         shutdown => 5000,
         type => worker,
         modules => [claws_xmpp_comp]} ].

start_link(#{host := H, domain := D} = ClawArgs)
        when H =:= undefined orelse D =:= undefined ->
    {ok, _PID} = claws_xmpp_comp:start_link(ClawArgs);
start_link(ClawArgs) ->
    {ok, PID} = claws_xmpp_comp:start_link(ClawArgs),
    ok = claws_xmpp_comp:connect(),
    {ok, PID}.

init([]) ->
    {ok, #state{}}.

handle_info({connected, _Claw}, State) ->
    {noreply, State};

handle_info({disconnected, _Claw}, State) ->
    {noreply, State};

handle_info({received,
             #xmlel{name = <<"iq">>, attrs = Attrs,
                    children = [#xmlel{name = <<"ping">>}]},
             #via{}}, State) ->
    ID = proplists:get_value(<<"id">>, Attrs),
    From = proplists:get_value(<<"from">>, Attrs),
    To = proplists:get_value(<<"to">>, Attrs),
    send(iq_resp(To, From, ID)),
    {noreply, State};

handle_info({received, _Packet, #via{}}, State) ->
    {noreply, State};

handle_info({received, _Packet}, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

send(Packet) ->
    snatch:send(Packet, <<>>).

iq_resp(From, To, ID) ->
    <<"<iq type='result' "
          "id='", ID/binary, "' "
          "from='", From/binary, "' "
          "to='", To/binary, "'/>">>.
