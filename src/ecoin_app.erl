-module(ecoin_app).

-behaviour(application).

-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    {ok, IP} = ecoin_util:get_external_ip(),
    application:set_env(ecoin, ip, IP),
    ecoin_sup:start_link().

stop(_State) ->
    ok.
