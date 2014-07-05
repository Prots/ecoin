-module(config).

-export([network/0,
         protocol_version/0,
         port/0,
         predefined_peers/0,
         dns/0,
         outgoing_limit/0,
         connection_timeout/0, 
         services/0,
         user_agent/0,
         relay/0]).

-include("ecoin.hrl").

network() ->
    conf(network, main).

protocol_version() -> 
    conf(protocol_version, 70002).

port() ->
    conf(port, 8333).

outgoing_limit() ->
    conf(outgoing_limit, 10).

connection_timeout() ->
    conf(connection_timeout, 10000).

user_agent() ->
    conf(user_agent, <<"berl">>).

services() -> 
    conf(services, []).

relay() -> 
    conf(relay, true).

predefined_peers() ->
    conf(peers, []).

dns() ->
    conf(dns, []).

conf(Key, Def) ->
    application:get_env(ecoin, Key, Def).
