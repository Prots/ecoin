-module(locator).

-export([pack/1,
         unpack/2]).

-include("ecoin.hrl").

%% @doc Pack a getblocks or getheaders message
pack(#getblocks{proto_ver=ProtoVer, locator=Locator, hash_stop=HashStop}) ->
    pack(ProtoVer, Locator, HashStop);
pack(#getheaders{proto_ver=ProtoVer, locator=Locator, hash_stop=HashStop}) ->
    pack(ProtoVer, Locator, HashStop).

%% @doc Pack a locator message (getblocks/getheaders)
pack(ProtoVer, Locator, HashStop) ->
    [<<ProtoVer:32/little>>,
     protocol:pack_var_uint(length(Locator)),
     Locator,
     HashStop].

%% @doc Unpack a locator message (getblocks/getheaders)
unpack(MessageType, Binary) ->
    {ProtoVer, Locator, HashStop} = unpack(Binary),
    case MessageType of
        getblocks  -> #getblocks{proto_ver = ProtoVer,
                                 locator   = Locator,
                                 hash_stop = HashStop};
        getheaders -> #getheaders{proto_ver = ProtoVer,
                                  locator   = Locator,
                                  hash_stop = HashStop}
    end.

unpack(<<ProtoVer:32/little, Binary0/binary>>) ->
    {Count, Binary1} = protocol:unpack_var_uint(Binary0),
    LocatorSize = Count * 32,
    <<Locator:LocatorSize/binary, HashStop:4/binary>> = Binary1,
    {ProtoVer, unpack_hashes(Count, Locator), HashStop}.

unpack_hashes(0, <<>>) ->
    [];
unpack_hashes(Count, <<Hash:32/binary, Rest/binary>>) ->
    [Hash | unpack_hashes(Count-1, Rest)].
