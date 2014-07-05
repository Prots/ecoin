-module(getdata).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a getdata message
-spec encode(#getdata{}) -> iodata().
encode(#getdata{inventory = Inventory}) ->
    protocol:encode_array(Inventory, fun inv:encode_inv_vect/1).

%% @doc Decode a getdata message
-spec decode(binary()) -> #getdata{}.
decode(Binary) ->
    #getdata{
       inventory = protocol:decode_array(Binary, fun inv:decode_inv_vect/1)
      }.
