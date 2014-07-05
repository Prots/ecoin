-module(addr).

-export([encode/1,
         decode/1,
         encode_net_addr/1,
         decode_net_addr/1,
         encode_ipaddress/1,
         decode_ipaddress/1]).

-include("ecoin.hrl").

%% @doc Encode an addr message
-spec encode(#addr{}) -> iodata().
encode(#addr{addr_list = Addresses}) ->
    protocol:encode_list(Addresses, fun encode_net_addr/1).

%% @doc Decode an addr message
-spec decode(binary()) -> #addr{}.
decode(Binary) ->
    {AddrList, <<>>} = protocol:decode_list(Binary, fun decode_net_addr/1),
    #addr{addr_list = AddrList}.

%% @doc Encode an net_addr structure
-spec encode_net_addr(#net_addr{}) -> iodata().
encode_net_addr(#net_addr{
    time     = Time,
    services = Services,
    ip       = IPAddr,
    port     = Port
                  }) ->
    [
     <<(ecoin_util:timestamp_to_integer(Time)):32/little>>,
     version:encode_services(Services),
     encode_ipaddress(IPAddr),
     <<Port:16>>
    ].

%% @doc Decode an net_addr structure
-spec decode_net_addr(<<_:240, _:_*8>>) -> {#net_addr{}, binary()}.
decode_net_addr(<<Time:32/little,
                  Services:8/binary,
                  IPAddr:16/binary,
                  Port:16, Rest/binary>>) ->
    NetAddr = #net_addr{
                 time     = ecoin_util:integer_to_timestamp(Time),
                 services = version:decode_services(Services),
                 ip       = decode_ipaddress(IPAddr),
                 port     = Port
                },
    {NetAddr, Rest}.

%% @doc Encode an IPv4/IPv6 address
-spec encode_ipaddress(ipaddr()) -> <<_:128>>.
encode_ipaddress({I0,I1,I2,I3}) ->
    <<16#ffff:96, I0, I1, I2, I3>>;
encode_ipaddress({I0, I1, I2, I3, I4, I5, I6, I7}) ->
    <<I0:16, I1:16, I2:16, I3:16, I4:16, I5:16, I6:16, I7:16>>.

%% @doc Decode an IPv6 address
-spec decode_ipaddress(<<_:128>>) -> ipaddr().
decode_ipaddress(<<I0:16, I1:16, I2:16, I3:16, I4:16, I5:16, I6:16, I7:16>>) ->
    {I0, I1, I2, I3, I4, I5, I6, I7}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

decode_test() ->
    Binary = <<
               16#01,                                   % Number of addresses: 1
                                                        % addr(0):
               16#E215104D:32,                          %     Timestamp: 2010-12-21 02:50:10 GMT
               16#0100000000000000:64,                  %     Services:  [node_network]
               16#00000000000000000000FFFF0A000001:128, %     IP:        10.0.0.1
               16#208D:16                               %     Port:      8333
             >>,
    ExpectedNetAddr = #net_addr{
                         time     = {{2010, 12, 21}, {2, 50, 10}},
                         services = [node_network],
                         ip       = {0, 0, 0, 0, 0, 16#FFFF, 16#0A00, 16#0001},
                         port     = 8333
                        },
    Expected = [ExpectedNetAddr],
    #addr{addr_list = Result} = addr:decode(Binary),
    Result1 = lists:map(fun (#net_addr{time = T} = N) ->
                                N#net_addr{
                                  time = calendar:now_to_universal_time(T)
                                 }
                        end, Result),
    ?assertEqual(Expected, Result1).

-endif.
