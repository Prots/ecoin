-module(ecoin_protocol_tests).

-include_lib("eunit/include/eunit.hrl").

decode_varuint_test() ->
    Cases =
        [{16#00, ecoin_protocol:decode_varuint(<<0>>)},
         {16#fc, ecoin_protocol:decode_varuint(<<16#fc>>)},
         {16#fd, ecoin_protocol:decode_varuint(<<16#fd, 16#fd:16/little>>)},
         {16#ffff, ecoin_protocol:decode_varuint(<<16#fd, 16#ffff:16/little>>)},
         {16#10000,
          ecoin_protocol:decode_varuint(<<16#fe,
                                          16#10000:32/little>>)},
         {16#ffffffff,
          ecoin_protocol:decode_varuint(<<16#fe,
                                          16#ffffffff:32/little>>)},
         {16#100000000,
          ecoin_protocol:decode_varuint(<<16#ff,
                                          16#100000000:64/little>>)},
         {16#ffffffffffffffff,
          ecoin_protocol:decode_varuint(<<16#ff,
                                          16#ffffffffffffffff:64/little>>)}],
    lists:foreach(fun({Expected, Result}) -> ?assertEqual(Expected, Result) end,
                  Cases).
