-module(ecoin_pong).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a pong message
-spec encode(#pong{}) -> <<_:64>>.
encode(#pong{nounce = Nounce}) -> <<Nounce:64/little>>.

%% @doc Decode a pong message
-spec decode(<<_:64>>) -> #pong{}.
decode(<<Nounce:64/little>>) -> #pong{nounce = Nounce}.
