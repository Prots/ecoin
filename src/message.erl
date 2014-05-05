-module(message).

-export([send_message/2,
         receive_message/1]).

-include("ecoin.hrl").

%% @doc Send a message over a socket.
%%      Construct header and pack it before sending. 
send_message(Socket, Message) ->
    {ok, Network} = config:network(),
    Payload = list_to_binary(pack(Message)),
    Header  = #message_header{network  = Network,
                              command  = element(1, Message),
                              length   = byte_size(Payload),
                              checksum = compute_checksum(Payload)},
    ranch_tcp:send(Socket, [pack_header(Header), Payload]).

%% @doc Receive a message over a socket
%%      Receive header
%%      Receive payload
%%      Check checksum
%%      Unpack message
receive_message(Socket) ->
    Header  = receive_header(Socket),
    #message_header{command  = Command,
                    length   = Length,
                    checksum = Checksum} = Header,
    {ok, Payload} = case Length of
                        0 ->
                            {ok, <<>>};
                        _ ->
                            ranch_tcp:recv(Socket, Length, infinity)
                    end,
    true = check_checksum(Checksum, Payload),
    Message = unpack(Command, Payload),
    {ok, Header, Message}.

%% @doc Pack a message
pack(Version) when is_record(Version, version) ->
    version:pack(Version);
pack(VerAck) when is_record(VerAck, verack) ->
    [<<>>];
pack(Addr) when is_record(Addr, addr) ->
    addr:pack(Addr);
pack(Inventory) when is_record(Inventory, inv) ->
    inv:pack(Inventory);
pack(GetData) when is_record(GetData, getdata) ->
    inv:pack(GetData);
pack(NotFound) when is_record(NotFound, notfound) ->
    inv:pack(NotFound);
pack(GetBlocks) when is_record(GetBlocks, getblocks) -> 
    locator:pack(GetBlocks);
pack(GetHeaders) when is_record(GetHeaders, getheaders) ->
    locator:pack(GetHeaders);
pack(Tx) when is_record(Tx, tx) ->
    tx:pack(Tx);
pack(Block) when is_record(Block, block) ->
    block:pack(Block);
pack(Headers) when is_record(Headers, headers) ->
    block:pack_headers(Headers);
pack(GetAddr) when is_record(GetAddr, getaddr) ->
    [<<>>];
pack(MemPool) when is_record(MemPool, mempool) ->
    [<<>>];
pack(#ping{nounce=Nounce}) ->
    [protocol:pack_uint64_le(Nounce)];
pack(#pong{nounce=Nounce}) ->
    [protocol:pack_uint64_le(Nounce)];
pack(FilterLoad) when is_record(FilterLoad, filterload) ->
    filter:pack_filterload(FilterLoad);
pack(FilterAdd) when is_record(FilterAdd, filteradd) ->
    [FilterAdd#filteradd.data];
pack(FilterClear) when is_record(FilterClear, filterclear) ->
    [<<>>];
pack(MerkleBlock) when is_record(MerkleBlock, merkleblock) ->
    filter:pack_merkleblock(MerkleBlock);
pack(Alert) when is_record(Alert, alert) ->
    alert:pack(Alert).

%% @doc Unpack a message
unpack(version, Binary) -> 
    version:pack(Binary);
unpack(verack, <<>>) ->
    #verack{};
unpack(addr, Binary) ->
    addr:unpack(Binary);
unpack(inv, Binary) ->
    inv:unpack(inv, Binary);
unpack(getdata, Binary) ->
    inv:unpack(getdata, Binary);
unpack(notfound, Binary) ->
    inv:unpack(notfound, Binary);
unpack(getblocks, Binary) ->
    locator:unpack(getblocks, Binary);
unpack(getheaders, Binary) ->
    locator:unpack(getheaders, Binary);
unpack(tx, Binary) ->
    tx:unpack(Binary);
unpack(block, Binary) ->
    block:unpack(Binary);
unpack(headers, Binary) ->
    block:unpack_headers(Binary);
unpack(getaddr, <<>>) ->
    #getaddr{};
unpack(mempool, <<>>) ->
    #mempool{};
unpack(ping, <<Nounce:64/little>>) ->
    #ping{nounce=Nounce};
unpack(pong, <<Nounce:64/little>>) ->
    #pong{nounce=Nounce};
unpack(filterload, Binary) ->
    filter:unpack_filterload(Binary);
unpack(filteradd, Data) ->
    #filteradd{data=Data};
unpack(filterclear, <<>>) ->
    #filterclear{};
unpack(merkleblock, Binary) ->
    filter:unpack_merkleblock(Binary);
unpack(alert, Binary) ->
    alert:unpack(Binary).
    
%% ---------------------------------------------------------------------------- 
%% Header
%% ---------------------------------------------------------------------------- 

%% @doc Receive a header
receive_header(Socket) ->
    {ok, Header} = ranch_tcp:recv(Socket, 24, infinity),
    unpack_header(Header).

%% @doc Pack a message header
pack_header(#message_header{network=Network, command=Command,
                            length=Length, checksum=Checksum}) ->
    [pack_magic(Network),
     pack_command(Command),
     <<Length:32/little>>,
     Checksum].

%% @doc Unpack a message header
unpack_header(<<Magic:4/binary, Command:12/binary,
                Length:32/little, Checksum:4/binary>>) ->
    #message_header{network  = unpack_magic(Magic),
                    command  = unpack_command(Command),
                    length   = Length,
                    checksum = Checksum}.

%% @doc Unpack a command
unpack_command(Command) ->
    WithoutPadding = hd(binary:split(Command, <<0>>)),
    binary_to_existing_atom(WithoutPadding, latin1).

%% @doc Pack a command atom
pack_command(Command) -> 
    Binary = atom_to_binary(Command, latin1),
    PaddingLength  = (12 - byte_size(Binary)) * 8,
    [Binary, <<0:PaddingLength>>].

%% @doc Compute the checksum of a payload
compute_checksum(Payload) ->
    Hash = crypto:hash(sha256, Payload),
    <<Checksum:4/binary, _/binary>> = crypto:hash(sha256, Hash),
    Checksum.

%% @doc Verify the checksum of a payload
check_checksum(Checksum, Payload) ->
    Checksum == compute_checksum(Payload).

%% @doc Pack the network from the magic bytes
unpack_magic(<<16#f9, 16#be, 16#b4, 16#d9>>) -> main;
unpack_magic(<<16#fa, 16#bf, 16#b5, 16#da>>) -> testnet;
unpack_magic(<<16#0b, 16#11, 16#09, 16#07>>) -> testnet3;
unpack_magic(<<16#f9, 16#be, 16#b4, 16#fe>>) -> namecoin.

%% @doc The magic bytes for the different networks
pack_magic(main)     -> <<16#f9, 16#be, 16#b4, 16#d9>>;
pack_magic(testnet)  -> <<16#fa, 16#bf, 16#b5, 16#da>>;
pack_magic(testnet3) -> <<16#0b, 16#11, 16#09, 16#07>>;
pack_magic(namecoin) -> <<16#f9, 16#be, 16#b4, 16#fe>>.
