-module(ecoin_block).

-export([new/6,
         genesis/0,
         genesis/1,
         genesis_raw/0,
         genesis_raw/1,
         encode/1,
         decode/1,
         build_merkle_root/1,
         hash/1,
         pp/1]).

-include("ecoin.hrl").

%% @doc Construct a new block
-spec new(uinteger(), hash(), timestamp(), uinteger(), uinteger(), [#tx{}]) ->
    #block{}.
new(Version, PrevBlock, Timestamp, Bits, Nounce, Txns) ->
    HashTx = fun(Tx) -> ecoin_crypto:hash256(ecoin_tx:encode(Tx)) end,
    MerkleRoot = build_merkle_root(lists:map(HashTx, Txns)),
    #block{version     = Version,
           prev_block  = PrevBlock,
           merkle_root = MerkleRoot,
           timestamp   = Timestamp,
           bits        = Bits,
           nounce      = Nounce,
           txns        = array:from_list(Txns)}.

%% @doc Construct the genesis block in the current network
-spec genesis() -> #block{}.
genesis() ->
    genesis(ecoin_config:network()).

%% @doc Construct the genesis block in the given network
-spec genesis(network()) -> #block{}.
genesis(main) ->
    Version   = 1,
    PrevBlock = <<0:32/unit:8>>,
    Timestamp = {1231, 6505, 0},
    Bits      = 16#1D00FFFF,
    Nounce    = 2083236893,
    Txns      = [ecoin_tx:genesis()],
    new(Version, PrevBlock, Timestamp, Bits, Nounce, Txns).

%% @doc Construct the genesis block in the current network (raw)
-spec genesis_raw() -> <<_:1720>>.
genesis_raw() -> genesis_raw(ecoin_config:network()).

%% @doc Construct the genesis block in the given network (raw)
-spec genesis_raw(network()) -> <<_:1720>>.
genesis_raw(Network) -> encode(genesis(Network)).

%% @doc Hash the block header to determine the block hash
-spec hash(#block{}) -> hash().
hash(Block) ->
    ecoin_crypto:hash256(encode(Block#block{txns = undefined})).

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
     <<Version:32/little,
       PrevBlock:32/binary,
       MerkleRoot:32/binary,
       (ecoin_util:timestamp_to_integer(Timestamp)):32/little,
       Bits:32/little,
       Nounce:32/little,
       (ecoin_protocol:encode_array(Txns, fun ecoin_tx:encode/1))/binary>>.

%% @doc Decode a block message
-spec decode(binary()) -> {#block{}, binary()} | #block{}.
decode(<<Version:32/little,
         PrevBlock:32/binary,
         MerkleRoot:32/binary,
         Timestamp0:32/little,
         Bits:32/little,
         Nounce:32/little, Binary/binary>>) ->
    Tx = fun ecoin_tx:decode/1,
    {Txns, Rest} = ecoin_protocol:decode_array(Binary, Tx),
    Timestamp = ecoin_util:integer_to_timestamp(Timestamp0),
    Block = #block{version     = Version,
                   prev_block  = PrevBlock,
                   merkle_root = MerkleRoot,
                   timestamp   = Timestamp,
                   bits        = Bits,
                   nounce      = Nounce,
                   txns        = Txns
                  },
    case byte_size(Rest) == 0 of
        true  -> Block;
        false -> {Block, Rest}
    end.

%% @doc Pretty print a block
-spec pp(#block{}) -> binary().
pp(Block) ->
    #block{
        version     = Version,
        prev_block  = PrevBlock,
        merkle_root = MerkleRoot,
        timestamp   = Timestamp,
        bits        = Bits,
        nounce      = Nounce,
        txns        = Txns
    } = Block,
    <<"BLOCK:\n"
      "Version:        ", (integer_to_binary(Version))/binary, "\n",
      "Previous block: ", (ecoin_util:bin_to_hexstr(PrevBlock))/binary, "\n",
      "Merkle root:    ", (ecoin_util:bin_to_hexstr(MerkleRoot))/binary, "\n",
      "Timestamp:      ", (ecoin_util:timestamp_to_binary(Timestamp))/binary, "\n",
      "Bits:           ", (integer_to_binary(Bits))/binary, "\n",
      "Nounce:         ", (integer_to_binary(Nounce))/binary, "\n",
      (lists:map(fun tx:pp/1, array:to_list(Txns)))/binary>>.

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

-spec calc_hash(pos_integer(), uinteger(), [hash()]) -> hash().
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

-include_lib("eunit/include/eunit.hrl").

genesis_raw_data() ->
    <<
      16#01000000000000000000000000000000:128,
      16#00000000000000000000000000000000:128,
      16#000000003BA3EDFD7A7B12B27AC72C3E:128,
      16#67768F617FC81BC3888A51323A9FB8AA:128,
      16#4B1E5E4A29AB5F49FFFF001D1DAC2B7C:128,
      16#01010000000100000000000000000000:128,
      16#00000000000000000000000000000000:128,
      16#000000000000FFFFFFFF4D04FFFF001D:128,
      16#0104455468652054696D65732030332F:128,
      16#4A616E2F32303039204368616E63656C:128,
      16#6C6F72206F6E206272696E6B206F6620:128,
      16#7365636F6E64206261696C6F75742066:128,
      16#6F722062616E6B73FFFFFFFF0100F205:128,
      16#2A01000000434104678AFDB0FE554827:128,
      16#1967F1A67130B7105CD6A828E03909A6:128,
      16#7962E0EA1F61DEB649F6BC3F4CEF38C4:128,
      16#F35504E51EC112DE5C384DF7BA0B8D57:128,
      16#8A4C702B6BF11D5FAC00000000:104
    >>.

block_encode_test() ->
    Raw = genesis_raw_data(),
    Enc = encode(genesis()),
    ct:pal("raw ~p~nenc: ~p", [Raw, Enc]),
    ?assertEqual(Raw, Enc).
