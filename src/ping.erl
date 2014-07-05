-module(ping).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a ping message
-spec encode(#ping{}) -> <<_:64>>.
encode(#ping{nounce = Nounce}) ->
    Nounce.

%% @doc Decode a ping message
-spec decode(<<_:64>>) -> #ping{}.
decode(Nounce) when byte_size(Nounce) == 8 ->
    #ping{nounce = Nounce}.
