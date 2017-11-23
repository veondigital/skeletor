-module('{{name}}_snatch_tests').
-author('{{author_email}}').

-include_lib("eunit/include/eunit.hrl").
-include_lib("fast_xml/include/fxml.hrl").

-define(XMPP_DOMAIN, <<"alice.localhost">>).

-import(test_common, [start_prometheus/0, stop_prometheus/0]).

custom_test_() ->
    {setup,
        fun() ->
            ok = start_prometheus(),
            {ok, _} = {{name}}_snatch:start_link(?XMPP_DOMAIN)
        end,
        fun(_) ->
            ok = stop_prometheus()
        end,
        snatch_fun_test:check([
            "xmpp_ping"
        ])}.
