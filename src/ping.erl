-module(ping).

-export([new/0,
         encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Create a new ping message
-spec new() -> #ping{}.
new() ->
    #ping{nounce = ecoin_util:nounce(8)}.

%% @doc Encode a ping message
-spec encode(#ping{}) -> <<_:64>>.
encode(#ping{nounce = Nounce}) ->
    <<Nounce:64/little>>.

%% @doc Decode a ping message
-spec decode(<<_:64>>) -> #ping{}.
decode(<<Nounce:64/little>>) ->
    #ping{nounce = Nounce}.
