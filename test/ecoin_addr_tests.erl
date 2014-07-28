-module(ecoin_addr_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ecoin.hrl").

encode_decode_net_addr_test() ->
    RawNetAddr = <<16#E215104D:32,                          %     Timestamp: 2010-12-21 02:50:10 GMT
                   16#0100000000000000:64,                  %     Services:  [node_network]
                   16#00000000000000000000FFFF0A000001:128, %     IP:        10.0.0.1
                   16#208D:16>>,                            %     Port:      8333
    ExpectedNetAddr = #net_addr{
                         time     = {1292,899810,0},
                         services = [node_network],
                         ip       = {0, 0, 0, 0, 0, 16#FFFF, 16#0A00, 16#0001},
                         port     = 8333
                        },
    ?assertEqual(ExpectedNetAddr, ecoin_addr:decode_net_addr(RawNetAddr)).
