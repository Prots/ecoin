-module(ecoin_headers).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a headers message
-spec encode(#headers{}) -> binary().
encode(#headers{headers = Headers}) ->
    EncodeBlock = fun ecoin_block:encode/1,
    ecoin_protocol:encode_array(Headers, EncodeBlock).

%% @doc Decode a headers message
-spec decode(binary()) -> #headers{}.
decode(Binary) ->
    DecodeBlock = fun ecoin_block:decode/1,
    #headers{headers = protocol:decode_array(Binary, DecodeBlock)}.
