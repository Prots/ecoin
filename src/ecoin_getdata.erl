-module(ecoin_getdata).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a getdata message
-spec encode(#getdata{}) -> binary().
encode(#getdata{inventory = Inventory}) ->
    EncodeInv = fun inv:encode_inv_vect/1,
    ecoin_protocol:encode_array(Inventory, EncodeInv).

%% @doc Decode a getdata message
-spec decode(binary()) -> #getdata{}.
decode(Binary) ->
    DecodeInv = fun inv:decode_inv_vect/1,
    #getdata{inventory = ecoin_protocol:decode_array(Binary, DecodeInv)}.
