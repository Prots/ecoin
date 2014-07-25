-module(ecoin_inv).

-export([encode/1,
         decode/1,
         encode_inv_vect/1,
         decode_inv_vect/1,
         encode_object_type/1,
         decode_object_type/1]).

-include("ecoin.hrl").

%% @doc Encode an inv message
-spec encode(#inv{}) -> binary().
encode(#inv{inventory = Inventory}) ->
    EncodeInvVect = fun encode_inv_vect/1,
    protocol:encode_array(Inventory, EncodeInvVect).

%% @doc Decode an inv message
-spec decode(binary()) -> #inv{}.
decode(Binary) ->
    DecodeInvVect = fun decode_inv_vect/1,
    Inventory = protocol:decode_array(Binary, DecodeInvVect),
    #inv{inventory = Inventory}.

%% @doc Encode an inv_vect structure
-spec encode_inv_vect(#inv_vect{})   -> binary().
encode_inv_vect(#inv_vect{type = ObjectType, hash = Hash}) ->
    <<(encode_object_type(ObjectType)):32/little, Hash/binary>>.

%% @doc Decode an inv_vect structure
-spec decode_inv_vect(<<_:288, _:_*8>>) -> {#inv_vect{}, binary()} | #inv_vect{}.
decode_inv_vect(<<ObjectType:32/little, Hash:32/binary, Rest/binary>>) ->
    InvVect = #inv_vect{type = decode_object_type(ObjectType), hash = Hash},
    case byte_size(Rest) == 0 of
        true  -> InvVect;
        false -> {InvVect, Rest}
    end.

%% @doc Encode an object type
-spec encode_object_type(object_type()) -> 0..3.
encode_object_type(error)          -> ?OBJECT_TYPE_ERROR;
encode_object_type(transaction)    -> ?OBJECT_TYPE_MSG_TX;
encode_object_type(block)          -> ?OBJECT_TYPE_MSG_BLOCK;
encode_object_type(filtered_block) -> ?OBJECT_TYPE_MSG_FILTERED_BLOCK.

%% @doc Decode an object type
-spec decode_object_type(0..3) -> object_type().
decode_object_type(?OBJECT_TYPE_ERROR)              -> error;
decode_object_type(?OBJECT_TYPE_MSG_TX)             -> transaction;
decode_object_type(?OBJECT_TYPE_MSG_BLOCK)          -> block;
decode_object_type(?OBJECT_TYPE_MSG_FILTERED_BLOCK) -> filtered_block.
