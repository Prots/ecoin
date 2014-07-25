-module(ecoin_notfound).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a notfound message
-spec encode(#notfound{}) -> binary().
encode(#notfound{inventory = Inventory}) ->
    EncodeInvVect = fun ecoin_inv:encode_inv_vect/1,
    ecoin_protocol:encode_array(Inventory, EncodeInvVect).

%% @doc Decode a notfound message
-spec decode(binary()) -> #notfound{}.
decode(Binary) ->
    DecodeInvVect = fun ecoin_inv:decode_inv_vect/1,
    InvArr = protocol:decode_array(Binary, DecodeInvVect),
    #notfound{inventory = InvArr}.
