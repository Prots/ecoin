-module(getblocks).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a getblocks message
-spec encode(#getblocks{}) -> iodata().
encode(#getblocks{
          version   = Version,
          locator   = Locator,
          hash_stop = HashStop
         }) ->
    locator:encode(Version, Locator, HashStop).

%% @doc Decode a getblocks message
-spec decode(binary()) -> #getblocks{}.
decode(Binary) ->
    {Version, Locator, HashStop} = locator:decode(Binary),
    #getblocks{
       version   = Version,
       locator   = Locator,
       hash_stop = HashStop
      }.
