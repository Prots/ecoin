-module(ecoin_message).

-export([send/2,
         recv/1,
         new/1,
         new/2,
         encode/1,
         decode/1]).

-include("ecoin.hrl").

%% @doc Send a message over a socket.
%%      Construct header and pack it before sending.
-spec send(socket(), #message{}) -> ok;
          (socket(), payload())  -> ok.
send(Socket, Message) when is_record(Message, message) ->
    ok = ranch_tcp:send(Socket, encode(Message)),
    #message{command = Command, length = Length} = Message,
    ecoin_stats:sent(Socket, Command, Length),
    ok;
send(Socket, Payload) ->
    send(Socket, new(Payload)).

%% @doc Receive a message over a socket
%%      Receive header
%%      Receive payload
%%      Check checksum
%%      Decode message
-spec recv(socket()) -> #message{}.
recv(Socket) ->
    recv(Socket, infinity).

-spec recv(socket(), timeout()) -> #message{}.
recv(Socket, Timeout) ->
    Network = ecoin_config:network(),
    {ok, MessageHeader} = ranch_tcp:recv(Socket, 24, Timeout),
    #message{
      network  = Network,
      command  = Command,
      length   = Length,
      checksum = Checksum
      } = Message = decode(MessageHeader),
    Payload = case Length of
                  0 ->
                      true = command_has_no_payload(Command),
                      true = checksum_is_valid(Checksum, <<>>),
                      undefined;
                  _ ->
                      true = command_has_payload(Command),
                      {ok, Data} = ranch_tcp:recv(Socket, Length, Timeout),
                      true = checksum_is_valid(Checksum, Data),
                      (cmd_to_mod(Command)):decode(Data)
              end,
    ecoin_stats:received(Socket, Command, Length),
    Message#message{payload = Payload}.

%% @doc Create a new message with given payload on main network
-spec new(payload()) -> #message{}.
new(Payload) ->
    new(ecoin_config:network(), Payload).

%% @doc Create a new message with given payload and network
-spec new(network(), payload()) -> #message{}.
new(Network, Command) when is_atom(Command) ->
    #message{
       network  = Network,
       command  = Command,
       length   = 0,
       checksum = compute_checksum(<<>>),
       payload  = <<>>
      };
new(Network, Payload) ->
    Command = element(1, Payload),
    EncodedPayload = (cmd_to_mod(Command)):encode(Payload),
    #message{
       network  = Network,
       command  = Command,
       length   = byte_size(EncodedPayload),
       checksum = compute_checksum(EncodedPayload),
       payload  = EncodedPayload
      }.

%% @doc Encode a message
-spec encode(#message{}) -> message_bin().
encode(#message{network  = Network,
                command  = Command,
                length   = Length,
                checksum = Checksum,
                payload  = Payload}) ->
     <<(encode_network(Network)):32/little,
     (encode_command(Command))/binary,
     Length:32/little,
     Checksum/binary,
     Payload/binary>>.

%% @doc Decode a message header
-spec decode(message_header_bin()) -> #message{}.
decode(<<Magic:32/little,
         Command:12/binary,
         Length:32/little,
         Checksum:4/binary>>) ->
    #message{network  = decode_network(Magic),
             command  = decode_command(Command),
             length   = Length,
             checksum = Checksum}.

%% @doc Encode the network magic bytes
-spec encode_network(network()) -> magic().
encode_network(main)     -> ?NETWORK_MAIN;
encode_network(testnet)  -> ?NETWORK_TESTNET;
encode_network(testnet3) -> ?NETWORK_TESTNET3;
encode_network(namecoin) -> ?NETWORK_NAMECOIN.

%% @doc Decode the magic bytes to get the network
-spec decode_network(magic()) -> network().
decode_network(?NETWORK_MAIN)     -> main;
decode_network(?NETWORK_TESTNET)  -> testnet;
decode_network(?NETWORK_TESTNET3) -> testnet3;
decode_network(?NETWORK_NAMECOIN) -> namecoin.

%% @doc Encode a command binary
-spec encode_command(command()) -> command_bin().
encode_command(Command) ->
    CommandBin = atom_to_binary(Command, utf8),
    PadLength = 12 - byte_size(CommandBin),
    <<CommandBin/binary, ?PAD(PadLength)/binary>>.

%% @doc Decode a command
-spec decode_command(command_bin()) -> command().
decode_command(CommandBin) when byte_size(CommandBin) == 12 ->
    Command = binary_to_existing_atom(hd(binary:split(CommandBin, <<0>>)), utf8),
    true = lists:member(Command, commands()),
    Command.

%% ----------------------------------------------------------------------------
%% Internal
%% ----------------------------------------------------------------------------

%% @doc All bitcoin messages (commands)
-spec commands() -> [command()].
commands() ->
    commands_with_payload() ++ commands_without_payload().

%% @doc Check if a command should have a payload
-spec command_has_payload(command_with_payload())    -> true;
                         (command_without_payload()) -> false.
command_has_payload(Command) ->
    lists:member(Command, commands_with_payload()).

%% @doc All commands that has a payload
-spec commands_with_payload() -> [command_with_payload()].
commands_with_payload() ->
    [version, addr, inv, getdata, notfound, getblocks, getheaders, tx, block,
     headers, ping, pong, reject, filterload, filteradd, merkleblock, alert].

%% @doc Check to see if a command shouldn't have a payload
-spec command_has_no_payload(command_without_payload()) -> true;
                            (command_with_payload())    -> false.
command_has_no_payload(Command) ->
    lists:member(Command, commands_without_payload()).

%% @doc All commands that don't carry a payload
-spec commands_without_payload() -> [command_without_payload()].
commands_without_payload() ->
    [verack, getaddr, mempool, filterclear].

%% @doc Compute the checksum of a payload
-spec compute_checksum(binary()) -> checksum().
compute_checksum(Payload) ->
    binary:part(ecoin_crypto:hash256(Payload), 0, 4).

%% @doc Verify the checksum of a payload
-spec checksum_is_valid(checksum, binary()) -> boolean().
checksum_is_valid(Checksum, Payload) ->
    Checksum == compute_checksum(Payload).

%% @doc Compute the module for this command
-spec cmd_to_mod(atom()) -> atom().
cmd_to_mod(Command) ->
    CommandBin = atom_to_binary(Command, utf8),
    binary_to_existing_atom(<<"ecoin_", CommandBin/binary>>, utf8).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

decode_header_test() ->
    Hdr1 = <<16#F9BEB4D9:32,                 % Network: main
             16#76657273696F6E0000000000:96, % Command: version
             16#64000000:32,                 % Length: 100
             16#3B648D5A:32>>,               % Checksum
    Hdr2 = <<16#F9BEB4D9:32,                 % Network: main
             16#76657261636B000000000000:96, % Command: verack
             16#00000000:32,                 % Length:  0
             16#5DF6E0E2:32>>,               % Checksum
    Hdr3 = <<16#F9BEB4D9:32,                 % Network: main
             16#616464720000000000000000:96, % Command: addr
             16#1F000000:32,                 % Length:  31
             16#ED52399B:32>>,               % Checksum
    Exp1 = #message{network  = main,
                    command  = version,
                    length   = 100,
                    checksum = <<16#3B648D5A:32>>},
    Exp2 = #message{network  = main,
                    command  = verack,
                    length   = 0,
                    checksum = <<16#5DF6E0E2:32>>},
    Exp3 = #message{network  = main,
                    command  = addr,
                    length   = 31,
                    checksum = <<16#ED52399B:32>>},
    Cases = [{Exp1, Hdr1}, {Exp2, Hdr2}, {Exp3, Hdr3}],
    [?assertEqual(Exp, decode(Hdr)) || {Exp, Hdr} <- Cases].

command_test() ->
    Cases = [{version,     <<"version",     0:40>>},
             {verack,      <<"verack",      0:48>>},
             {addr,        <<"addr",        0:64>>},
             {inv,         <<"inv",         0:72>>},
             {getdata,     <<"getdata",     0:40>>},
             {notfound,    <<"notfound",    0:32>>},
             {getblocks,   <<"getblocks",   0:24>>},
             {getheaders,  <<"getheaders",  0:16>>},
             {tx,          <<"tx",          0:80>>},
             {block,       <<"block",       0:56>>},
             {headers,     <<"headers",     0:40>>},
             {getaddr,     <<"getaddr",     0:40>>},
             {mempool,     <<"mempool",     0:40>>},
             {ping,        <<"ping",        0:64>>},
             {pong,        <<"pong",        0:64>>},
             {reject,      <<"reject",      0:48>>},
             {filterload,  <<"filterload",  0:16>>},
             {filteradd,   <<"filteradd",   0:24>>},
             {filterclear, <<"filterclear", 0>>},
             {merkleblock, <<"merkleblock", 0>>}],
    lists:foreach(fun({Command, CommandBin}) ->
                          ?assertEqual(Command, decode_command(CommandBin)),
                          ?assertEqual(CommandBin, encode_command(Command))
                  end, Cases).
-endif.
