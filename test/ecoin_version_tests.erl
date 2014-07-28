-module(ecoin_version_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ecoin.hrl").

decode_test() ->
    RawVersion = <<16#62EA0000:32,
                   16#0100000000000000:64,
                   16#11B2D05000000000:64,
                   16#010000000000000000000000000000000000FFFF000000000000:26/unit:8,
                   16#010000000000000000000000000000000000FFFF000000000000:26/unit:8,
                   16#3B2EB35D8CE61765:64,
                   16#0F2F5361746F7368693A302E372E322F:16/unit:8,
                   16#C03E0300:32,
                   1>>,
    ExpNetAddr = #net_addr{time     = undefined,
                           services = [node_network],
                           ip       = {0, 0, 0, 0, 0, 16#FFFF, 0, 0},
                           port     = 0},
    Expected = #version{version      = 60002,
                        services     = [node_network],
                        timestamp    = {1355,854353,0},
                        addr_from    = ExpNetAddr,
                        addr_recv    = ExpNetAddr,
                        nounce       = 7284544412836900411,
                        user_agent   = <<"/Satoshi:0.7.2/">>,
                        start_height = 212672,
                        relay        = true},
    ?assertEqual(Expected,   ecoin_version:decode(RawVersion)),
    ?assertEqual(RawVersion, ecoin_version:encode(Expected)).

