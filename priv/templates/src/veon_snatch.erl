-module('{{name}}_snatch').
-author('{{author_email}}').

-behaviour(gen_server).

-include_lib("snatch/include/snatch.hrl").
-include_lib("fast_xml/include/fxml.hrl").

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
-define(DEFAULT_PORT_XMPP, 8888).
-define(DEFAULT_PORT_REST, 80).
-define(DEFAULT_DOMAIN, undefined).
-define(DEFAULT_PASS, <<"secret">>).
-define(DEFAULT_SCHEMA, "http").
-define(DEFAULT_MAX_SESSIONS, 10).
-define(DEFAULT_PIPELINE_SIZE, 1).

specs(Opts) ->
    SnatchArgs = [claws_xmpp_comp, ?MODULE],
    XmppOpts = proplists:get_value(xmpp, Opts, []),
    KafkaOpts = proplists:get_value(kafka, Opts, []),
    RestOpts = proplists:get_value(rest, Opts, []),
    Domain = proplists:get_value(domain, XmppOpts, ?DEFAULT_DOMAIN),
    XmppArgs = [#{
        host => proplists:get_value(host, XmppOpts, ?DEFAULT_HOST),
        port => proplists:get_value(port, XmppOpts, ?DEFAULT_PORT_XMPP),
        domain => Domain,
        password => proplists:get_value(password, XmppOpts, ?DEFAULT_PASS)
    }],
    KafkaArgs = [#{
        endpoints => proplists:get_value(endpoints, KafkaOpts, undefined),
        in_topics => proplists:get_value(in_topics, KafkaOpts, []),
        out_topic => proplists:get_value(out_topic, KafkaOpts, undefined),
        out_partition => proplists:get_value(out_partition, KafkaOpts, 0),
        trimmed => proplists:get_value(trimmed, KafkaOpts, false),
        raw => proplists:get_value(raw, KafkaOpts, false)
    }],
    RestArgs = [#{
        domain => proplists:get_value(domain, RestOpts, undefined),
        port => proplists:get_value(port, RestOpts, ?DEFAULT_PORT_REST),
        schema => proplists:get_value(schema, RestOpts, ?DEFAULT_SCHEMA),
        max_sessions => proplists:get_value(max_sessions, RestOpts,
                                            ?DEFAULT_MAX_SESSIONS),
        max_pipeline_size => proplists:get_value(max_pipeline_size, RestOpts,
                                                 ?DEFAULT_PIPELINE_SIZE)
    }],
    [ #{ id => '{{name}}_xmpp_snatch',
         start => {snatch, start_link, SnatchArgs},
         restart => permanent,
         shutdown => 5000,
         type => worker,
         modules => [snatch]},
      #{ id => '{{name}}_xmpp_claw',
         start => {?MODULE, start_claws_xmpp_comp_link, XmppArgs},
         restart => permanent,
         shutdown => 5000,
         type => worker,
         modules => [claws_xmpp_comp]},
      #{ id => '{{name}}_snatch_router',
         start => {?MODULE, start_link, [Domain]},
         restart => permanent,
         shutdown => 5000,
         type => worker,
         modules => [?MODULE]} ] ++
    case RestArgs of
        [#{domain := undefined}] ->
            [];
        _ ->
            [#{ id => '{{name}}_rest_claw',
                start => {claws_rest, start_link, RestArgs},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [claws_rest]}]
    end ++
    case KafkaArgs of
        [#{endpoints := undefined}] ->
            [];
        _ ->
            [#{ id => '{{name}}_kafka_claw',
                start => {claws_kafka, start_link, KafkaArgs},
                restart => permanent,
                shutdown => 5000,
                type => worker,
                modules => [claws_kafka]}]
    end.

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

handle_cast({send, Packet}, #state{domain = Domain} = State) ->
    prometheus_summary:observe(xmpp_kb_sent, byte_size(Packet)),
    snatch:send(Packet, Domain),
    {noreply, State};

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

handle_info({received, _Packet, #via{claws = claws_xmpp_comp}}, State) ->
    %% TODO code about incoming stanzas from XMPP
    {noreply, State};

handle_info({received, _Packet, #via{claws = claws_kafka}}, State) ->
    %% TODO code about incoming messages from Kafka
    {noreply, State};

handle_info({received, _Packet, #via{claws = claws_rest}}, State) ->
    %% TODO code about incoming messages from REST
    {noreply, State};

handle_info({received, _Packet}, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

send(Packet) when is_binary(Packet) ->
    gen_server:cast(?MODULE, {send, Packet}).

iq_resp(From, To, ID) ->
    <<"<iq type='result' "
          "id='", ID/binary, "' "
          "from='", From/binary, "' "
          "to='", To/binary, "'/>">>.
