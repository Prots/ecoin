-module(pong).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a pong message
-spec encode(#pong{}) -> <<_:64>>.
encode(#pong{nounce = Nounce}) ->
    Nounce.

%% @doc Decode a pong message
-spec decode(<<_:64>>) -> #pong{}.
decode(Nounce) when byte_size(Nounce) == 8 ->
    #pong{nounce = Nounce}.
