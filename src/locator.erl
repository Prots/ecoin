-module(locator).

-export([encode/3,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a locator message
-spec encode(uinteger(), locator(), hash()) -> iodata().
encode(Version, Locator, HashStop) ->
    [
     <<Version:32/little>>,
     protocol:encode_array(Locator, fun(Hash) -> Hash end),
     HashStop
    ].

%% @doc Decode a locator message
-spec decode(binary()) -> {uinteger(), locator(), hash()}.
decode(<<Version:32/little, Binary/binary>>) ->
    {Count, Binary1} = protocol:decode_varuint(Binary),
    Size = Count * 32,
    <<Hashes:Size/binary, HashStop:4/binary>> = Binary1,
    {
     Version,
     protocol:decode_array(Hashes,
                           fun(<<Hash:32/binary, Rest/binary>>) ->
                                   {Hash, Rest}
                           end),
     HashStop
    }.
