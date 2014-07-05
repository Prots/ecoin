-module(headers).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a headers message
-spec encode(#headers{}) -> iodata().
encode(#headers{headers = Headers}) ->
    protocol:encode_array(Headers, fun block:encode/1).

%% @doc Decode a headers message
-spec decode(binary()) -> #headers{}.
decode(Binary) ->
    #headers{headers = protocol:decode_array(Binary, fun block:decode/1)}.
