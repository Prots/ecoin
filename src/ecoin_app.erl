-module(ecoin_app).

-behaviour(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->

    %% Fetch our external ip address and set it
    {ok, IP} = ecoin_util:get_external_ip(),
    config:set_ip(IP),

    ecoin_sup:start_link().

stop(_State) ->
    ok.
