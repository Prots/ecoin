-module(inv).

-export([encode/1,
         decode/2]).

-include("ecoin.hrl").

%% @doc Encode an inv message and it's parts
-spec encode(object_type() | #inv_vect{} | #inv{}) -> iodata().
encode(error) -> 0;
encode(transaction) -> 1;
encode(block) -> 2;
encode(filtered_block) -> 3;
encode(#inv_vect{type = ObjectType, hash = Hash}) ->
    [<<(encode(ObjectType)):32/little>>, Hash];
encode(#inv{inventory=Inventory}) ->
    Count = protocol:encode_varuint(length(Inventory)),
    
    [Count,
     lists:map(fun ({_Index, InvVect}) -> encode(InvVect) end,
               array:to_list(Inventory))].

%% @doc Decode an inv message and it's parts
-spec decode(uinteger() | binary()) -> object_type() | #inv_vect{} | #inv{}.
decode(0) -> error;
decode(1) -> transaction;
decode(2) -> block;
decode(3) -> filtered_block;
decode(<<ObjectType:32/little, Hash:32/binary>>) ->
    #inv_vect{type = decode(ObjectType), hash = Hash};
decode(Binary0) ->
    {Count, Binary1} = protocol:decode_varuint(Binary0),
    #inv{inventory = array:from_list(decode(Count, Binary1))}.

-spec decode(uinteger(), binary()) -> [#inv_vect{}].
decode(0, <<>>) ->
    [];
decode(Count, <<Binary:36/binary, Rest/binary>>) ->
    [decode(Binary) | decode(Count-1, Rest)].
