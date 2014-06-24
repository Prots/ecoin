-module(locator).

-export([encode/3,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a locator message
-spec encode(uinteger(), locator(), hash()) -> iodata().
encode(Version, Locator, HashStop) ->
    [<<Version:32/little>>,
     protocol:encode_varuint(length(Locator)),
     Locator,
     HashStop].

%% @doc Decode a locator message
-spec decode(binary()) -> {uinteger(), locator(), hash()}.
decode(<<Version:32/little, Binary0/binary>>) ->
    {Count, Binary1} = protocol:decode_varuint(Binary0),
    Size = Count * 32,
    <<Hashes:Size/binary, HashStop:4/binary>> = Binary1,
    {Version, decode(Count, Hashes), HashStop}.

-spec decode(uinteger(), binary()) -> [hash()].
decode(0, <<>>) ->
    [];
decode(Count, <<Hash:32/binary, Rest/binary>>) ->
    [Hash | decode(Count-1, Rest)].
