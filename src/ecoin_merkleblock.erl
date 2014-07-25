-module(ecoin_merkleblock).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a merkleblock message
-spec encode(#merkleblock{}) -> binary().
encode(#merkleblock{
          version     = Version,
          prev_block  = PrevBlock,
          merkle_root = MerkleRoot,
          timestamp   = Timestamp,
          bits        = Bits,
          nounce      = Nounce,
          total_txns  = TotalTxns,
          hashes      = Hashes,
          flags       = Flags
         }) ->
     <<Version:32/little,
     PrevBlock/binary,
     MerkleRoot/binary,
     (ecoin_util:ts_to_int(Timestamp)):32/little,
     Bits:32/little,
     Nounce:32/little,
     TotalTxns:32/little,
     Hashes/binary,
     (encode_flags(Flags))>>.

%% @doc Decode a merkleblock message
-spec decode(binary()) -> #merkleblock{}.
decode(Binary) ->
    <<Version:32/little,
      PrevBlock:32/binary,
      MerkleRoot:32/binary,
      Timestamp:32/little,
      Bits:32/little,
      Nounce:32/little,
      TotalTxns:32/little, Binary1/binary>> = Binary,
    {Count, Binary2} = ecoin_protocol:decode_varint(Binary1),
    Size = Count * 32,
    <<Hashes:Size/binary, Flags/binary>> = Binary2,
    #merkleblock{
       version = Version,
       prev_block = PrevBlock,
       merkle_root = MerkleRoot,
       timestamp = ecoin_util:integer_to_timestamp(Timestamp),
       bits        = Bits,
       nounce      = Nounce,
       total_txns  = TotalTxns,
       hashes      = ecoin_protocol:split(Hashes, 32),
       flags       = decode_flags(Flags)
      }.

%% @doc Encode flags
-spec encode_flags([0 | 1]) -> binary().
encode_flags(Flags) ->
    BitString = list_to_bitstring(lists:map(fun(Flag) -> <<Flag:1>> end, Flags)),
    Padding = bit_size(BitString) rem 8,
    <<BitString/bitstring, 0:Padding>>.

%% @doc Decode flags
-spec decode_flags(binary()) -> [0 | 1].
decode_flags(<<>>) -> [];
decode_flags(<<Flag:1, Rest/bitstring>>) ->
    [Flag | decode_flags(Rest)].
