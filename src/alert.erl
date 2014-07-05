-module(alert).

-export([decode/1]).

-include("ecoin.hrl").

%% @doc Decode an alert message, verifying the signature as well
-spec decode(binary()) -> #alert{}.
decode(Binary) ->
    {Payload, Binary1} = protocol:decode_varbin(Binary),
    {Signature, <<>>}  = protocol:decode_varbin(Binary1),
    true = crypto:verify(ecdsa, sha256, Payload, Signature, ?CORE_PUB_KEY),
    decode_alert(Payload).

%% @doc Decode the alert message
-spec decode_alert(binary()) -> #alert{}.
decode_alert(Binary) ->
    <<
      Version:32/signed-little,
      RelayUntil:64/signed-little,
      Expiration:64/signed-little,
      ID:32/signed-little,
      Cancel:32/signed-little, Binary1/binary
    >> = Binary,
    Int32 = fun (<<Int32:32/signed-little, Rest/binary>>) -> {Int32, Rest} end,
    {SetCancel, Binary2} = protocol:decode_list(Binary1, Int32),
    <<
      MinVer:32/signed-little,
      MaxVer:32/signed-little, Binary3/binary
    >> = Binary2,
    undefined.
