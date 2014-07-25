-module(ecoin_filterload).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a filterload message
-spec encode(#filterload{}) -> binary().
encode(#filterload{
          filter = Filter,
          n_hash_funs = NHashFuns,
          n_tweak = NTweak,
          n_flags = NFlags
         }) ->
    <<Filter/binary,
      NHashFuns:32/little,
      NTweak:32/little,
      (encode_n_flags(NFlags))/binary>>.

%% @doc Decode a filterload message
-spec decode(binary()) -> #filterload{}.
decode(Binary) ->
    FilterSize = byte_size(Binary) - 9,
    <<
      Filter:FilterSize/binary,
      NHashFuns:32/little,
      NTweak:32/little,
      NFlags
    >> = Binary,
    #filterload{
       filter      = Filter,
       n_hash_funs = NHashFuns,
       n_tweak     = NTweak,
       n_flags     = decode_n_flags(NFlags)
      }.

%% @doc Encode n_flags
-spec encode_n_flags(filter_flags()) -> byte().
encode_n_flags([]) -> 0;
encode_n_flags([Flag | Flags]) ->
    encode_n_flags(Flag) bor encode_n_flags(Flags);
encode_n_flags(none)          -> ?BLOOM_UPDATE_NONE;
encode_n_flags(all)           -> ?BLOOM_UPDATE_ALL;
encode_n_flags(p2pubkey_only) -> ?BLOOM_UPDATE_ONLY_P2PUBKEY_ONLY.

%% @doc Decode n_flags
-spec decode_n_flags(byte()) -> [filter_flag()].
decode_n_flags(Flags) ->
    [Flag || Flag <- all_filter_flags(),
             ecoin_util:in_mask(encode_n_flags(Flag), Flags)].

%% @doc All filter flags
-spec all_filter_flags() -> filter_flags().
all_filter_flags() -> [none, all, p2pubkey_only].
