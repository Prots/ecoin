-module(notfound).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a notfound message
-spec encode(#notfound{}) -> iodata().
encode(#notfound{inventory = Inventory}) ->
    protocol:encode_array(Inventory, fun inv:encode_inv_vect/1).

%% @doc Decode a notfound message
-spec decode(binary()) -> #notfound{}.
decode(Binary) ->
    #notfound{
       inventory = protocol:decode_array(Binary, fun inv:decode_inv_vect/1)
      }.
