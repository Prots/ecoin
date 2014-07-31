-module(ecoin_tx).

-export([genesis/0,
         encode/1,
         decode/1,
         encode_tx_in/1,
         decode_tx_in/1,
         encode_tx_out/1,
         decode_tx_out/1,
         encode_outpoint/1,
         decode_outpoint/1,
         pp/1]).

-include("ecoin.hrl").

%% @doc Retrive the genesis transaction
-spec genesis() -> #tx{}.
genesis() ->
    {SigScript, PKScript} = ecoin_script:genesis(),
    {SigScriptBin, PKScriptBin} = {ecoin_script:encode(SigScript),
                                   ecoin_script:encode(PKScript)},
    Outpoint = #outpoint{hash = <<0:256>>, index = 16#FFFFFFFF},
    TxIn = array:from_list([#tx_in{previous_output = Outpoint,
                                   sig_script      = SigScriptBin,
                                   sequence        = 16#FFFFFFFF}]),
    TxOut = array:from_list([#tx_out{value     = 5000000000,
                                     pk_script = PKScriptBin}]),
    set_hash(#tx{version   = 1,
                 tx_in     = TxIn,
                 tx_out    = TxOut,
                 lock_time = 0}).

%% @doc Set the hash attribute of a newly created tx
-spec set_hash(#tx{}) -> #tx{}.
set_hash(Tx) ->
    Tx#tx{hash = ecoin_crypto:hash256(encode(Tx))}.

%% @doc Encode a tx message
-spec encode(#tx{}) -> iodata().
encode(#tx{version   = Version,
          tx_in     = TxIn,
          tx_out    = TxOut,
          lock_time = LockTime0}) ->
    LockTime = case LockTime0 of
                   {timestamp, Timestamp} -> ecoin_util:ts_to_int(Timestamp);
                   {block, BlockNumber}   -> BlockNumber;
                   _                      -> LockTime0
               end,
     <<Version:32/little,
       (ecoin_protocol:encode_array(TxIn,  fun encode_tx_in/1))/binary,
       (ecoin_protocol:encode_array(TxOut, fun encode_tx_out/1))/binary,
       LockTime:32/little>>.

%% @doc Decode a tx message
-spec decode(binary()) -> {#tx{}, binary()} | #tx{}.
decode(<<Version:32/little, Binary/binary>>) ->
    DecTxIn = fun decode_tx_in/1,
    DecTxOut = fun decode_tx_out/1,
    {TxIn,  Binary1} = ecoin_protocol:decode_array(Binary, DecTxIn),
    {TxOut, Binary2} = ecoin_protocol:decode_array(Binary1, DecTxOut),
    <<LockTime:32, Rest/binary>> = Binary2,
    Tx = #tx{version   = Version,
             tx_in     = TxIn,
             tx_out    = TxOut,
             lock_time = LockTime},
    ecoin_protocol:empty_rest({set_hash(Tx), Rest}).

%% @doc Encode a tx_in structure
-spec encode_tx_in(#tx_in{}) -> binary().
encode_tx_in(#tx_in{previous_output = PreviousOutput,
                    sig_script      = Script,
                    sequence        = Sequence}) ->
     <<(encode_outpoint(PreviousOutput))/binary,
       (ecoin_protocol:encode_varbin(Script))/binary,
       Sequence:32/little>>.

%% @doc Decode a tx_in structure
-spec decode_tx_in(binary()) -> {#tx_in{}, binary()} | #tx_in{}.
decode_tx_in(<<Outpoint:36/binary, Binary/binary>>) ->
    {Script, Binary1} = ecoin_protocol:decode_varbin(Binary),
    <<Sequence:32/little, Rest/binary>> = Binary1,
    TxIn = #tx_in{previous_output = decode_outpoint(Outpoint),
                  sig_script      = Script,
                  sequence        = Sequence},
    ecoin_protocol:empty_rest({TxIn, Rest}).

%% @doc Encode an outpoint structure
-spec encode_outpoint(#outpoint{}) -> <<_:288>>.
encode_outpoint(#outpoint{hash = Hash, index = Index}) ->
     <<Hash:32/binary, Index:32/little>>.

%% @doc Decode a outpoint structure
-spec decode_outpoint(<<_:288>>) -> #outpoint{}.
decode_outpoint(<<Hash:32/binary, Index:32/little>>) ->
    #outpoint{hash = Hash, index = Index}.

%% @doc Encode a tx_out structure
-spec encode_tx_out(#tx_out{}) -> binary().
encode_tx_out(#tx_out{value = Value, pk_script = Script}) ->
     <<Value:64/little, (ecoin_protocol:encode_varbin(Script))/binary>>.

%% @doc Decode a tx_out structure
-spec decode_tx_out(binary()) -> {#tx_out{}, binary()} | #tx_out{}.
decode_tx_out(<<Value:64/little, Binary/binary>>) ->
    TxOut = #tx_out{value = Value},
    case ecoin_protocol:decode_varbin(Binary) of
        {PKScript, Rest} -> {TxOut#tx_out{pk_script = PKScript}, Rest};
        PKScript         -> TxOut#tx_out{pk_script = PKScript}
    end.

%% @doc Pretty print a transaction
-spec pp(#tx{}) -> binary().
pp(#tx{version   = Version,
       tx_in     = TxIn,
       tx_out    = TxOut,
       lock_time = LockTime}) ->
   <<"TRANSACTION: \n",
     "Version: ", (integer_to_binary(Version))/binary, "\n",
     "Txns in: \n",
     (lists:map(fun pp/1, array:to_list(TxIn)))/binary,
     "Txns out: \n",
     (lists:map(fun pp/1, array:to_list(TxOut)))/binary,
     "Locktime: ", (integer_to_binary(LockTime))/binary, "\n">>;
pp(#tx_in{previous_output = Outpoint,
          sig_script      = Script,
          sequence        = Sequence}) ->
   #outpoint{hash = Hash, index = Index} = Outpoint,
   <<"TXIN: \n",
     "Previous output: ",
     (integer_to_binary(Index))/binary, "-",
     (ecoin_util:bin_to_hexstr(Hash))/binary,
     "Script: ", (ecoin_util:bin_to_hexstr(Script))/binary, "\n",
     "Sequence: ", (integer_to_binary(Sequence))/binary, "\n">>;
pp(#tx_out{value = Value, pk_script = Script}) ->
    <<"TXOUT: \n",
      "Value: ", (integer_to_binary(Value))/binary, "\n",
      "Script: ", (ecoin_util:bin_to_hexstr(Script))/binary, "\n">>.

