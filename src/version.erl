-module(version).

-export([encode/1,
         decode/1,
         all_services/0,
         encode_service/1,
         encode_services/1,
         decode_services/1]).

-include("ecoin.hrl").

%% @doc Encode a version record, services or the relay boolean
-spec encode(#version{}) -> iodata().
encode(#version{
          version      = Version,
          services     = Services,
          timestamp    = Timestamp,
          addr_recv    = AddrRecv,
          addr_from    = AddrFrom,
          nounce       = Nounce,
          user_agent   = UserAgent,
          start_height = StartHeight,
          relay        = Relay
         }) ->
    RelayByte = case Relay of
                    true  -> 1;
                    false -> 0
                end,
    [
     <<Version:32/signed-little>>,
     encode_services(Services),
     <<(ecoin_util:timestamp_to_integer(Timestamp)):64/signed-little>>,
     encode_net_addr(AddrRecv),
     encode_net_addr(AddrFrom),
     <<Nounce:32/little>>,
     protocol:encode_varbin(UserAgent),
     <<StartHeight:32/signed-little>>,
     RelayByte
    ].

%% @doc Decode a version message or services.
-spec decode(binary()) -> #version{};
            (uinteger()) -> [services()].
decode(Binary) when is_binary(Binary) ->
    <<
      Version:32/signed-little,
      Services:8/binary,
      Timestamp:64/signed-little,
      AddrRecv:26/binary,
      AddrFrom:26/binary,
      Nounce:32/little, Binary1/binary
    >> = Binary,
    {UserAgent, Binary2} = protocol:decode_varbin(Binary1),
    <<StartHeight:32/signed-little,
      RelayByte/binary>> = Binary2,
    Relay = case RelayByte of
                <<1>> -> true;
                <<0>> -> false;
                <<>>  -> true
            end,
    #version{
       version      = Version,
       services     = decode_services(Services),
       timestamp    = ecoin_util:integer_to_timestamp(Timestamp),
       addr_recv    = decode_net_addr(AddrRecv),
       addr_from    = decode_net_addr(AddrFrom),
       nounce       = Nounce,
       user_agent   = UserAgent,
       start_height = StartHeight,
       relay        = Relay
      }.

%% @doc Special net_addr encode function
-spec encode_net_addr(#net_addr{}) -> binary().
encode_net_addr(NetAddr) ->
    NetAddr1 = NetAddr#net_addr{time = 0},
    <<_:32, Binary/binary>> = iolist_to_binary(addr:encode_net_addr(NetAddr1)),
    Binary.

%% @doc Special net_addr decode function
-spec decode_net_addr(<<_:240>>) -> #net_addr{}.
decode_net_addr(Binary) ->
    {NetAddr, <<>>} = addr:decode_net_addr(<<0:32, Binary/binary>>),
    NetAddr#net_addr{time = undefined}.

%% @doc All defined services
-spec all_services() -> services().
all_services() -> [node_network].

%% @doc Encode a service
-spec encode_service(service()) -> uinteger().
encode_service(node_network) -> ?SERVICE_NODE_NETWORK.

%% @doc Encode a services field
-spec encode_services(services()) -> <<_:64>>.
encode_services(Services) ->
    Int = lists:foldl(fun(Value, Acc) -> Value bor Acc end,
                      lists:map(fun encode/1, Services)),
    <<Int:64/little>>.

%% @doc Decode a services field
-spec decode_services(<<_:64>>) -> services().
decode_services(<<Services:64/little>>) ->
    [Service || Service <- all_services(),
                ecoin_util:in_mask(encode_service(Service), Services)].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

decode_test() ->
    Binary = <<
               16#62EA0000:32,
               16#0100000000000000:64,
               16#11B2D05000000000:64,
               16#010000000000000000000000000000000000FFFF000000000000:26/unit:8,
               16#010000000000000000000000000000000000FFFF000000000000:26/unit:8,
               16#3B2EB35D8CE61765:32,
               16#0F2F5361746F7368693A302E372E322F:16/unit:8,
               16#C03E0300:32
             >>,
    NetAddr = #net_addr{
                 time     = undefined,
                 services = [node_network],
                 ip       = {0, 0, 0, 0, 0, 16#FFFF, 0, 0},
                 port     = 0
                },
    Expected = #version{
                 version      = 60002,
                 services     = [node_network],
                 timestamp    = {{2012, 12, 18}, {18, 12, 33}},
                 addr_from    = NetAddr,
                 addr_recv    = NetAddr,
                 nounce       = 1696065164,
                 user_agent   = <<"/Satoshi:0.7.2/">>,
                 start_height = 212672,
                 relay        = true
                 },
    #version{timestamp = TS} = V  = version:decode(Binary),
    Version= V#version{timestamp = calendar:now_to_universal_time(TS)},
    ?assertEqual(Expected, Version).

-endif.
