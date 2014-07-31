-module(ecoin_reject).

-export([encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Encode a reject message
-spec encode(#reject{}) -> binary().
encode(#reject{message = Message,
               ccode   = CCode,
               reason  = Reason}) ->
    iolist_to_binary([ecoin_protocol:encode_varbin(Message),
                      CCode,
                      ecoin_protocol:encode_varbin(Reason)]).

%% @doc Decode a reject message
-spec decode(binary()) -> #reject{}.
decode(Binary) ->
    {Message, <<CCode, Binary1/binary>>} = ecoin_protocol:decode_varbin(Binary),
    Reason = ecoin_protocol:decode_varbin(Binary1),
    #reject{
       message = Message,
       ccode   = CCode,
       reason  = Reason
      }.
