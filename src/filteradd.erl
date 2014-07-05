-module(filteradd).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a filteradd message
-spec encode(#filteradd{}) -> binary().
encode(#filteradd{data = Data}) -> Data.

%% @doc Decode a filteradd message
-spec decode(binary()) -> #filteradd{}.
decode(Data) when Data =< 520 -> #filteradd{data = Data}.
