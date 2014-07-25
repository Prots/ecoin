-module(ecoin_config).

-export([network/0,
         protocol_version/0,
         set_ip/1,
         ip/0,
         default_port/0,
         default_port/1,
         port/0,
         predefined_peers/0,
         dns/0,
         dns_limit/0,
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

set_ip(IP) ->
    application:set_env(ecoin, ip, IP).

ip() ->
    conf(ip, {127,0,0,1}).

default_port() ->
    default_port(network()).

default_port(main) ->
    8333;
default_port(testnet3) ->
    18333.

port() ->
    conf(port, 8333).

outgoing_limit() ->
    conf(outgoing_limit, 10).

connection_timeout() ->
    conf(connection_timeout, 10000).

user_agent() ->
    conf(user_agent, <<"ecoin-dev">>).

services() ->
    conf(services, []).

relay() ->
    conf(relay, true).

predefined_peers() ->
    conf(peers, []).

dns() ->
    conf(dns, []).

dns_limit() ->
    conf(dns_limit, 25).

conf(Key, Def) ->
    application:get_env(ecoin, Key, Def).
