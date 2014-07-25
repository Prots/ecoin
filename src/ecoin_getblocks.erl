-module(ecoin_getblocks).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a getblocks message
-spec encode(#getblocks{}) -> binary().
encode(#getblocks{
          version   = Version,
          locator   = Locator,
          hash_stop = HashStop
         }) ->
    ecoin_locator:encode(Version, Locator, HashStop).

%% @doc Decode a getblocks message
-spec decode(binary()) -> #getblocks{}.
decode(Binary) ->
    {Version, Locator, HashStop} = ecoin_locator:decode(Binary),
    #getblocks{
       version   = Version,
       locator   = Locator,
       hash_stop = HashStop
      }.
