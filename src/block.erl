-module(block).

-export([genesis/0,
         genesis/1,
         encode/1,
         decode/1,
         build_merkle_root/1]).

-include("ecoin.hrl").


%% @doc Construct the genesis block in the current network
-spec genesis() -> #block{}.
genesis() ->
    genesis(config:network()).

%% @doc Construct the genesis block in the given network
-spec genesis(network()) -> #block{}.
genesis(main) ->
    CoinBaseParam = iolist_to_binary(
                      [script:encode(
                         [
                          {push, 486604799},
                          {push, 4}
                         ]),
                       <<"The Times 03/Jan/2009 Chancellor on "
                         "brink of second bailout for banks">>
                      ]),
    NullOutpoint = #outpoint{
                      hash  = <<0:32/unit:8>>,
                      index = -1
                     },
    CoinBaseTxIn = #tx_in{
                      previous_output = NullOutpoint,
                      script          = CoinBaseParam,
                      sequence        = 16#FFFFFFFF
                     },
    TxOut = #tx_out{
               value = 5000000000,
               script = <<>>
              },
    CoinBaseTx = #tx{
                    version  = 1,
                    tx_in    = [CoinBaseTxIn],
                    tx_out   = [TxOut],
                    lock_time = 0
                   },
    TxHash = ecoin_crypto:hash256(tx:encode(CoinBaseTx)),
    #block{
        version     = 1,
        prev_block  = <<0:32/unit:8>>,
        merkle_root = build_merkle_root([TxHash]),
        timestamp   = {1231, 6505, 0},
        bits        = 16#1D00FFFF,
        nounce      = 2083236893,
        txns        = array:from_list([CoinBaseTx])
      }.

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
-spec decode(binary()) -> {#block{}, binary()}.
decode(<<Version:32/little,
         PrevBlock:32/binary,
         MerkleRoot:32/binary,
         Timestamp:32/little,
         Bits:32/little,
         Nounce:32/little, Binary/binary>>) ->
    {Txns, Binary1} = protocol:decode_array(Binary, fun tx:decode/1),
    {
     #block{
        version     = Version,
        prev_block  = PrevBlock,
        merkle_root = MerkleRoot,
        timestamp   = ecoin_util:integer_to_timestamp(Timestamp),
        bits        = Bits,
        nounce      = Nounce,
        txns        = Txns
       },
     Binary1
    }.

%% @doc Given the number of transactions, calculate the height of the tree.
-spec calc_tree_height(integer()) -> integer().
calc_tree_height(NumTxns) ->
    round(math:log(NumTxns)/math:log(2)).

-spec calc_tree_width(integer(), integer()) -> integer().
calc_tree_width(NumTxns, Height) ->
    (NumTxns + (1 bsl Height) - 1) bsr Height.

-spec build_merkle_root([hash()]) -> hash().
build_merkle_root(Hashes) ->
    Height = calc_tree_height(length(Hashes)),
    calc_hash(Height, 0, Hashes).

-spec calc_hash(pos_integer(), non_neg_integer(), [binary()]) -> binary().
calc_hash(0, Pos, Hashes) ->
    lists:nth(Pos + 1, Hashes);
calc_hash(Height, Pos, Hashes) ->
    Left = calc_hash(Height - 1, Pos * 2, Hashes),
    Right = case Pos * 2 + 1 < calc_tree_width(length(Hashes), Height - 1) of
                true ->
                    calc_hash(Height - 1, Pos * 2 + 1, Hashes);
                false ->
                    Left
            end,
    ecoin_crypto:hash256([Left, Right]).
