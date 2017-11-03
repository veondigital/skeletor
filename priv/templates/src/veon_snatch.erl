-module('{{name}}_snatch').
-author('{{author_email}}').

-behaviour(gen_server).

-include_lib("snatch/include/snatch.hrl").
-include_lib("xmpp/include/xmpp.hrl").

-export([specs/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2,
         send/1,
         start_link/1,
         start_claws_xmpp_comp_link/1]).

-record(state, {
    domain :: binary()
}).

-define(DEFAULT_HOST, undefined).
-define(DEFAULT_PORT, 8888).
-define(DEFAULT_DOMAIN, undefined).
-define(DEFAULT_PASS, <<"secret">>).

specs(Opts) ->
    SnatchArgs = [claws_xmpp_comp, ?MODULE],
    Domain = proplists:get_value(domain, Opts, ?DEFAULT_DOMAIN),
    ClawArgs = [#{
        host => proplists:get_value(host, Opts, ?DEFAULT_HOST),
        port => proplists:get_value(port, Opts, ?DEFAULT_PORT),
        domain => Domain,
        password => proplists:get_value(password, Opts, ?DEFAULT_PASS)
    }],
    [ #{ id => '{{name}}_xmpp_snatch',
         start => {snatch, start_link, SnatchArgs},
         restart => permanent,
         shutdown => 5000,
         type => worker,
         modules => [snatch]},
      #{ id => '{{name}}_xmpp_claw',
         start => {?MODULE, start_claws_xmpp_comp_link, ClawArgs},
         restart => permanent,
         shutdown => 5000,
         type => worker,
         modules => [claws_xmpp_comp]},
      #{ id => '{{name}}_snatch_router',
         start => {?MODULE, start_link, [Domain]},
         restart => permanent,
         shutdown => 5000,
         type => worker,
         modules => [?MODULE]} ].

start_claws_xmpp_comp_link(#{host := H, domain := D} = ClawArgs)
        when H =:= undefined orelse D =:= undefined ->
    {ok, _PID} = claws_xmpp_comp:start_link(ClawArgs);
start_claws_xmpp_comp_link(ClawArgs) ->
    {ok, PID} = claws_xmpp_comp:start_link(ClawArgs),
    ok = claws_xmpp_comp:connect(),
    {ok, PID}.

start_link(Domain) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Domain], []).

init([Domain]) ->
    {ok, #state{domain = Domain}}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({connected, _Claw}, State) ->
    {noreply, State};

handle_info({disconnected, _Claw}, State) ->
    {noreply, State};

handle_info({received,
             #xmlel{name = <<"iq">>, attrs = Attrs,
                    children = [#xmlel{name = <<"ping">>}]},
             #via{}}, State) ->
    prometheus_counter:inc(stanza_counter),
    ID = proplists:get_value(<<"id">>, Attrs),
    From = proplists:get_value(<<"from">>, Attrs),
    To = proplists:get_value(<<"to">>, Attrs),
    send(iq_resp(To, From, ID)),
    {noreply, State};

handle_info({received, _Packet, #via{}}, State) ->
    {noreply, State};

handle_info({received, _Packet}, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

send(Packet) when is_binary(Packet) ->
    prometheus_summary:observe(xmpp_kb_sent, byte_size(Packet)),
    snatch:send(Packet, <<>>).

iq_resp(From, To, ID) ->
    <<"<iq type='result' "
          "id='", ID/binary, "' "
          "from='", From/binary, "' "
          "to='", To/binary, "'/>">>.
