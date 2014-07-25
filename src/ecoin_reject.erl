-module(ecoin_reject).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a reject message
-spec encode(#reject{}) -> binary().
encode(#reject{message = Message,
               ccode   = CCode,
               reason  = Reason}) ->
    [
     protocol:encode_varbin(Message),
     CCode,
     protocol:encode_varbin(Reason)
    ].

%% @doc Decode a reject message
-spec decode(binary()) -> #reject{}.
decode(Binary) ->
    {Message, <<CCode, Binary1/binary>>} = protocol:decode_varbin(Binary),
    {Reason, <<>>} = protocol:decode_varbin(Binary1),
    #reject{
       message = Message,
       ccode   = CCode,
       reason  = Reason
      }.
