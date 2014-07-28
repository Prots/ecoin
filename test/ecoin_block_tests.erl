-module(ecoin_block_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ecoin.hrl").

encode_decode_genesis_test() ->
    RawBlock0 =
        <<16#01000000000000000000000000000000:128,
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
          16#8A4C702B6BF11D5FAC00000000:104>>,

    GenesisBlock = ecoin_block:genesis(main),
    HexSigScript = <<"04FFFF001D0104455468652054696D65"
                     "732030332F4A616E2F32303039204368"
                     "616E63656C6C6F72206F6E206272696E"
                     "6B206F66207365636F6E64206261696C"
                     "6F757420666F722062616E6B73">>,
    HexPKScript = <<"4104678AFDB0FE5548271967F1A67130B"
                    "7105CD6A828E03909A67962E0EA1F61DE"
                    "B649F6BC3F4CEF38C4F35504E51EC112D"
                    "E5C384DF7BA0B8D578A4C702B6BF11D5F"
                    "AC">>,
    RawSigScript = ecoin_util:hexstr_to_bin(HexSigScript),
    RawPKScript  = ecoin_util:hexstr_to_bin(HexPKScript),
    RawBlock1 = <<% version
                  16#01000000:32,
                  % prev_block
                  16#0:256,
                  % merkle_root
                  16#3BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4A:256,
                  % timestamp
                  16#29AB5F49:32,
                  % bits
                  16#FFFF001D:32,
                  % nounce
                  16#1DAC2B7C:32,
                  % transactions
                  % number of transactions
                  16#01,
                  % tx[1]
                  % version
                  16#01000000:32,
                  % number of inputs
                  16#01,
                  % tx_in[0]
                  % outpoint - hash
                  16#0:256,
                  % outpoint - index
                  16#FFFFFFFF:32,
                  % script_sig length
                  16#4D,
                  % script_sig
                  RawSigScript/binary,
                  % sequence
                  16#FFFFFFFF:32,
                  % number of outputs
                  16#01,
                  % tx_out[0]
                  % value
                  16#00F2052A01000000:64,
                  % pk_script length
                  16#43,
                  % pk_script
                  RawPKScript/binary,
                  % lock_time
                  0:32>>,
    ?assertEqual(byte_size(RawBlock0), binary:longest_common_prefix([RawBlock0, RawBlock1])),
    ?assertEqual(GenesisBlock, ecoin_block:decode(RawBlock1)),
    ?assertEqual(RawBlock1, ecoin_block:encode(GenesisBlock)).
