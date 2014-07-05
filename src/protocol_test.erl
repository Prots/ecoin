-module(protocol_test).

-include_lib("eunit/include/eunit.hrl").

decode_varuint_test_() ->
    [
     ?_assertEqual({16#00, <<>>},
                   protocol:decode_varuint(<<0>>)),
     ?_assertEqual({16#fc, <<>>},
                   protocol:decode_varuint(<<16#fc>>)),
     ?_assertEqual({16#fd, <<>>},
                   protocol:decode_varuint(<<16#fd, 16#fd:16/little>>)),
     ?_assertEqual({16#ffff, <<>>},
                   protocol:decode_varuint(<<16#fd, 16#ffff:16/little>>)),
     ?_assertEqual({16#10000, <<>>},
                   protocol:decode_varuint(<<16#fe, 16#10000:32/little>>)),
     ?_assertEqual({16#ffffffff, <<>>},
                   protocol:decode_varuint(<<16#fe, 16#ffffffff:32/little>>)),
     ?_assertEqual({16#100000000, <<>>},
                   protocol:decode_varuint(<<16#ff, 16#100000000:64/little>>)),
     ?_assertEqual({16#ffffffffffffffff, <<>>},
                   protocol:decode_varuint(<<16#ff, 16#ffffffffffffffff:64/little>>))
    ].
