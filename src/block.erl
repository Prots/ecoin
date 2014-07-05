-module(block).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a block message
-spec encode(#block{}) -> iodata().
encode(#block{
          version     = Version,
          prev_block  = PrevBlock,
          merkle_root = MerkleRoot,
          timestamp   = Timestamp,
          bits        = Bits,
          nounce      = Nounce,
          txns        = Txns
         }) ->
    [
     <<Version:32/little>>,
     PrevBlock,
     MerkleRoot,
     <<
       (ecoin_util:timestamp_to_integer(Timestamp)):32/little,
       Bits:32/little,
       Nounce:32/little
     >>,
     protocol:encode_array(Txns, fun tx:encode/1)
    ].

%% @doc Decode a block message
-spec decode(binary()) -> #block{}.
decode(<<Version:32/little,
         PrevBlock:32/binary,
         MerkleRoot:32/binary,
         Timestamp:32/little,
         Bits:32/little,
         Nounce:32/little, Binary/binary>>) ->
    #block{
       version     = Version,
       prev_block  = PrevBlock,
       merkle_root = MerkleRoot,
       timestamp   = ecoin_util:integer_to_timestamp(Timestamp),
       bits        = Bits,
       nounce      = Nounce,
       txns        = protocol:decode_array(Binary, fun tx:decode/1)
      }.
