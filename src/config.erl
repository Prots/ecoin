-module(config).

-export([network/0,
         protocol_version/0,
         ip/0,
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
    get_config_value(network, main).


protocol_version() -> 
    70002.

ip() ->
    get_config_value(ip, {127,0,0,1}).

port() ->
    get_config_value(port, 8333).

outgoing_limit() ->
    get_config_value(outgoing_limit, 10).

connection_timeout() ->
    get_config_value(connection_timeout, 10000).

user_agent() ->
    {ok, Version} = application:get_key(vsn),
    "/berl:" ++ Version ++ "/" .

services() -> 
    [node_network].

relay() -> 
    true.

predefined_peers() ->
    get_config_value(peers, []).

dns() ->
    DefaultDNS = ["bitseed.xf2.org",
                  "dnsseed.bluematt.me",
                  "seed.bitcoin.sipa.be",
                  "dnsseed.bitcoin.dashjr.org",
                  "seed.bitcoinstats.com"],
    get_config_value(dns, DefaultDNS).

get_config_value(Key, Default) ->
    case application:get_env(Key) of
        {ok, Val} -> {ok, Val};
        _         -> {ok, Default}
    end.
