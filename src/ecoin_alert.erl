-module(ecoin_alert).

-export([decode/1]).

-include("ecoin.hrl").

%% @doc Decode an alert message, verifying the signature as well
-spec decode(binary()) -> #alert{}.
decode(Binary) ->
    {Payload, Binary1} = ecoin_protocol:decode_varbin(Binary),
    {Signature, <<>>}  = ecoin_protocol:decode_varbin(Binary1),
    true = crypto:verify(ecdsa, sha256, Payload, Signature, ?CORE_PUB_KEY),
    decode_alert(Payload).

%% @doc Decode the alert message
-spec decode_alert(binary()) -> #alert{}.
decode_alert(Binary) ->
    DecodeInt32 = fun (<<Int32:32/signed-little, Rest/binary>>) -> {Int32, Rest} end,
    DecodeVarBin = fun ecoin_protocol:decode_varbin/1,

    <<Version:32/signed-little,
      RelayUntil:64/signed-little,
      Expiration:64/signed-little,
      ID:32/signed-little,
      Cancel:32/signed-little, Binary1/binary>> = Binary,
    {SetCancel, Binary2} = ecoin_protocol:decode_list(Binary1, DecodeInt32),
    <<MinVer:32/signed-little,
      MaxVer:32/signed-little, Binary3/binary>> = Binary2,
    {SetSubVer, Binary4} = ecoin_protocol:decode_list(Binary3, DecodeVarBin),
    <<Priority:32/signed-little, Binary5/binary>> = Binary4,
    {Comment, Binary6} = ecoin_protocol:decode_varbin(Binary5),
    {StatusBar, Binary7} = ecoin_protocol:decode_varbin(Binary6),
    Reserved = ecoin_protocol:decode_varbin(Binary7),
    true = is_binary(Reserved),

    #alert{version     = Version,
           relay_until = RelayUntil,
           expiration  = Expiration,
           id          = ID,
           cancel      = Cancel,
           set_cancel  = SetCancel,
           min_ver     = MinVer,
           max_ver     = MaxVer,
           set_sub_ver = SetSubVer,
           priority    = Priority,
           comment     = Comment,
           status_bar  = StatusBar,
           reserved    = Reserved}.
