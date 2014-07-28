-module(ecoin_version).

-export([new/1,
         encode/1,
         decode/1,
         all_services/0,
         service_to_integer/1,
         encode_services/1,
         decode_services/1]).

-include("ecoin.hrl").

%% @doc Create a new version message depending on
%%      the configuration and the state of client(start_height).
-spec new(#net_addr{}) -> #version{}.
new(Peer) ->
    Services = ecoin_config:services(),
    #version{version   = ecoin_config:protocol_version(),
             services  = Services,
             timestamp = now(),
             addr_recv = Peer,
             addr_from = #net_addr{services = Services,
                                   ip       = ecoin_config:ip(),
                                   port     = ecoin_config:port()},
             nounce       = ecoin_util:nounce(4),
             user_agent   = ecoin_config:user_agent(),
             start_height = ecoin_blockchain:last_block(),
             relay        = ecoin_config:relay()}.

%% @doc Encode a version message
-spec encode(#version{}) -> binary().
encode(#version{version      = Version,
                services     = Services,
                timestamp    = Timestamp,
                addr_recv    = AddrRecv,
                addr_from    = AddrFrom,
                nounce       = Nounce,
                user_agent   = UserAgent,
                start_height = StartHeight,
                relay        = Relay}) ->
    RelayByte = case Relay of
                    true  -> 1;
                    false -> 0
                end,
    iolist_to_binary([<<Version:32/signed-little>>,
                      encode_services(Services),
                      <<(ecoin_util:ts_to_int(Timestamp)):64/signed-little>>,
                      encode_net_addr(AddrRecv),
                      encode_net_addr(AddrFrom),
                      <<Nounce:64/little>>,
                      ecoin_protocol:encode_varbin(UserAgent),
                      <<StartHeight:32/signed-little>>,
                      RelayByte]).

%% @doc Decode a version message
-spec decode(binary()) -> #version{}.
decode(Binary) ->
    <<Version:32/signed-little,
      Services:8/binary,
      Timestamp:64/signed-little,
      AddrRecv:26/binary,
      AddrFrom:26/binary,
      Nounce:64/little, Binary1/binary>> = Binary,
    {UserAgent, Binary2} = ecoin_protocol:decode_varbin(Binary1),
    <<StartHeight:32/signed-little,
      RelayByte/binary>> = Binary2,
    Relay = case RelayByte of
                <<1>> -> true;
                <<0>> -> false;
                <<>>  -> true
            end,
    #version{version      = Version,
             services     = decode_services(Services),
             timestamp    = ecoin_util:int_to_ts(Timestamp),
             addr_recv    = decode_net_addr(AddrRecv),
             addr_from    = decode_net_addr(AddrFrom),
             nounce       = Nounce,
             user_agent   = UserAgent,
             start_height = StartHeight,
             relay        = Relay}.

%% @doc Special net_addr encode function
-spec encode_net_addr(#net_addr{}) -> binary().
encode_net_addr(NetAddr) ->
    NetAddr1 = NetAddr#net_addr{time = {0, 0, 0}},
    <<_:32, Binary/binary>> = ecoin_addr:encode_net_addr(NetAddr1),
    Binary.

%% @doc Special net_addr decode function
-spec decode_net_addr(<<_:240>>) -> #net_addr{}.
decode_net_addr(Binary) ->
    NetAddr = ecoin_addr:decode_net_addr(<<0:32, Binary/binary>>),
    NetAddr#net_addr{time = undefined}.

%% @doc All defined services
-spec all_services() -> services().
all_services() -> [node_network].

%% @doc Cast an service atom into an integer
-spec service_to_integer(service()) -> uinteger().
service_to_integer(node_network) -> ?SERVICE_NODE_NETWORK.

%% @doc Encode a services field
-spec encode_services(undefined | services()) -> <<_:64>>.
encode_services(undefined) ->
    encode_services([]);
encode_services(Services) ->
    ServiceInts = lists:map(fun service_to_integer/1, Services),
    Int = lists:foldl(fun erlang:'bor'/2, 0, ServiceInts),
    <<Int:64/little>>.

%% @doc Decode a services field
-spec decode_services(<<_:64>>) -> services().
decode_services(<<Services:64/little>>) ->
    [Service || Service <- all_services(),
                ecoin_util:in_mask(service_to_integer(Service), Services)].
