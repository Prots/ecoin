-module(tx_tests).

-include_lib("eunit/include/eunit.erl").

encode_test() ->
    GB = block:genesis(main),
    Txns = GB#block.txns,
    Tx1 = array:get(0, Txns),
    HexScriptSig = <<
                     "04FFFF001D0104455468652054696D65"
                     "732030332F4A616E2F32303039204368"
                     "616E63656C6C6F72206F6E206272696E"
                     "6B206F66207365636F6E64206261696C"
                     "6F757420666F722062616E6B73"
                   >>,
    HexPkScript = <<
                    "4104678AFDB0FE5548271967F1A67130B"
                    "7105CD6A828E03909A67962E0EA1F61DE"
                    "B649F6BC3F4CEF38C4F35504E51EC112D"
                    "E5C384DF7BA0B8D578A4C702B6BF11D5F"
                    "AC"
                  >>,
    RawScriptSig = ecoin_util:hexstr_to_bin(HexScriptSig),
    RawPkScript = ecoin_util:hexstr_to_bin(HexPkScript),
    RawTx = <<
              % version
              16#01000000:32,
              % prev_block
              16#0:256
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
              RawScriptSig/binary
              % sequence
              16#FFFFFFFF:32,
              % number of outputs
              16#01,
              % tx_out[0]
              % pk_script length
              16#43,
              % pk_script
              RawPkScript/binary,
              % lock_time
              0:32
            >>,

