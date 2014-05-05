-module(tx).

-export([pack/1,
         unpack/1]).

-include("ecoin.hrl").

%% @doc Pack a tx message
pack(#tx{proto_ver=ProtoVer, in=TxIn, out=TxOut, lock_time=LockTime}) ->
    InCount  = protocol:pack_var_uint(length(TxIn)),
    OutCount = protocol:pack_var_uint(length(TxOut)),
    [<<ProtoVer:32/little>>,
     InCount,
     lists:map(fun pack_tx_in/1, TxIn),
     OutCount,
     lists:map(fun pack_tx_out/1, TxOut),
     <<LockTime:32/little>>].

%% @doc Unpack a tx message
unpack(<<ProtoVer:32/little, Binary0/binary>>) ->
    {InCount, Binary1} = protocol:unpack_var_uint(Binary0),
    {TxIn, Binary2} = unpack_tx_in(InCount, Binary1),
    {OutCount, Binary3} = protocol:unpack_var_uint(Binary2),
    {TxOut, <<LockTime:32/little>>} = unpack_tx_out(OutCount, Binary3),
    #tx{proto_ver=ProtoVer, in=TxIn, out=TxOut, lock_time=LockTime}.

pack_tx_in(#tx_in{prev_output={Hash, Index}, script=Script, sequence=Seq}) ->
    ScriptBin = script:pack(Script),
    ScriptSize = protocol:pack_var_uint(byte_size(ScriptBin)),
    [<<Hash:32/binary, Index:32/little>>,
     ScriptSize,
     ScriptBin,
     <<Seq:32/little>>].

unpack_tx_in(InCount, Binary) ->
    unpack_tx_in(InCount, Binary, []).

unpack_tx_in(0, Binary, Acc) ->
    {lists:reverse(Acc), Binary};
unpack_tx_in(InCount, Binary0, Acc) ->
    <<Hash:32/binary, Index:32/little, Binary1/binary>> = Binary0,
    {ScriptSize, Binary2} = protocol:unpack_var_uint(Binary1),
    <<ScriptBin:ScriptSize, Seq:32/little, Binary3/binary>> = Binary2,
    TxIn = #tx_in{prev_output = {Hash, Index}, 
                  script      = script:unpack(ScriptBin), 
                  sequence    = Seq},
    unpack_tx_in(InCount-1, Binary3, [TxIn|Acc]).

pack_tx_out(#tx_out{value=Value, script=Script}) ->
    [<<Value:64/little>>, script:pack(Script)].

unpack_tx_out(OutCount, Binary) ->
    unpack_tx_out(OutCount, Binary, []).
unpack_tx_out(0, Binary, Acc) ->
    {lists:reverse(Acc), Binary};
unpack_tx_out(OutCount, <<Value:64/little, Binary0/binary>>, Acc) ->
    {Script, Binary1} = script:unpack(Binary0),
    TxOut = #tx_out{value=Value, script=Script},
    unpack_tx_out(OutCount-1, Binary1, [TxOut|Acc]).
