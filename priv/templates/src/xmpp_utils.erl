-module(xmpp_utils).

-include("../include/executor.hrl").

-export([
	iq_result/1,
	iq_resp/3,
	iq_resp/4,
	iq_error/3,
	normalize_jid/1
]).

normalize_jid(JID) ->
    case JID of
        {undefined, D, undefined} ->
            <<D/binary>>;
        {N, D, undefined} ->
            <<N/binary,<<"@">>/binary,D/binary>>;
        {N, D, R} ->
            <<N/binary,<<"@">>/binary,D/binary,<<"/">>/binary,R/binary>>;
        BJID when is_binary(JID) ->
            BJID;
        LJID when is_list(JID) ->
            erlang:list_to_binary(LJID);
        _ ->
            undefined
    end.

iq_resp(From, To, ID) ->
    iq_resp(<<"result">>, From, To, ID).

iq_resp(Type, From, To, ID) ->
    <<"<iq type='", Type/binary, "' "
          "id='", ID/binary, "' "
          "from='", From/binary, "' "
          "to='", To/binary, "'/>">>.

iq_error(From, To, ID) ->
    iq_resp(<<"error">>, From, To, ID).

iq_result(#iq{from = From, to = To, id = ID}) ->
	iq_resp(To, From, ID).