-module(inv).

-export([encode/1,
         decode/1,
         encode_inv_vect/1,
         decode_inv_vect/1,
         encode_object_type/1,
         decode_object_type/1]).

-include("ecoin.hrl").

%% @doc Encode an inv message
-spec encode(#inv{}) -> iodata().
encode(#inv{inventory = Inventory}) ->
    protocol:encode_array(Inventory, fun encode/1).

%% @doc Decode an inv message
-spec decode(binary()) -> #inv{}.
decode(Binary) ->
    #inv{
       inventory = protocol:decode_array(Binary, 36, fun decode_inv_vect/1)
      }.

%% @doc Encode an inv_vect structure
-spec encode_inv_vect(#inv_vect{})   -> iodata().
encode_inv_vect(#inv_vect{type = ObjectType, hash = Hash}) ->
    [<<(encode_object_type(ObjectType)):32/little>>, Hash].

%% @doc Decode an inv_vect structure
-spec decode_inv_vect(<<_:288>>) -> #inv_vect{}.
decode_inv_vect(<<ObjectType:32/little, Hash:32/binary>>) ->
    #inv_vect{type = decode_object_type(ObjectType), hash = Hash}.

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
