-module(ecoin_tx_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ecoin.hrl").

decode_test() ->
    Hash = <<
             16#6DBDDB085B1D8AF75184F0BC01FAD58D:128,
             16#1266E9B63B50881990E4B40D6AEE3629:128
           >>,
    ScriptSig = <<
                  16#483045022100F3581E1972AE8AC7C736:128,
                  16#7A7A253BC1135223ADB9A468BB3A5923:128,
                  16#3F45BC578380022059AF01CA17D00E41:128,
                  16#837A1D58E97AA31BAE584EDEC28D35BD:128,
                  16#96923690913BAE9A0141049C02BFC97E:128,
                  16#F236CE6D8FE5D94013C721E915982ACD:128,
                  16#2B12B65D9B7D59E20A842005F8FC4E02:128,
                  16#532E873D37B96F09D6D4511ADA8F1404:128,
                  16#2F46614A4C70C0F14BEFF5:88
                >>,
    PkScript1 = <<
                  16#76A9141AA0CD1CBEA6E7458A7ABAD512:128,
                  16#A9D9EA1AFB225E88AC:72
                >>,
    PkScript2 = <<
                  16#76A9140EAB5BEA436A0484CFAB12485E:128,
                  16#FDA0B78B4ECC5288AC:72
                >>,
    Binary = <<
               16#01000000:32,                          % Version: 1
               16#01,                                   % Number of inputs: 1
                                                        % tx_in(0):
                                                        %     previous_output
               Hash/binary,                             %         hash
               16#00000000:32,                          %         index: 0
               16#8B,                                   %     Script length: 139
               ScriptSig/binary,                        %     scriptSig
               16#FFFFFFFF:32,                          %     sequence: FFFFFFFF
               16#02,                                   % Number of outputs: 2
                                                        % tx_out(0):
               16#404B4C0000000000:64,                  %     value: 5000000
               16#19,                                   %     Script length: 25
               PkScript1/binary,                        %     pk_script
                                                        % tx_out(1):
               16#80FAE9C700000000:64,                  %     value: 3354000000
               16#19,                                   %     Script length: 25
               PkScript2/binary,                        %     pk_script
               16#00000000:32                           % lock_time: 0
             >>,
    ExpectedOutpoint = #outpoint{
                          hash  = Hash,
                          index = 0
                         },
    ExpectedTxIn = #tx_in{
                      previous_output = ExpectedOutpoint,
                      sig_script          = ScriptSig,
                      sequence        = 16#FFFFFFFF
                     },
    ExpectedTxOuts = [
                      #tx_out{
                         value  = 5000000,
                         pk_script = PkScript1
                        },
                      #tx_out{
                         value  = 3354000000,
                         pk_script = PkScript2
                        }
                     ],
    Expected = #tx{
                  version   = 1,
                  tx_in     = array:from_list([ExpectedTxIn]),
                  tx_out    = array:from_list(ExpectedTxOuts),
                  lock_time = 0
                 },
    ?assertEqual(Expected, ecoin_tx:decode(Binary)).
