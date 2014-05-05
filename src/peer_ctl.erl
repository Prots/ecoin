%% @doc Controlling peer process
%%      This is processes initate new incoming
%%      and outgoing connection. It does the version
%%      handshake and validate that the connection
%%      should indeed be used. After the handshake is
%%      done it spawns a sending and a receiving process.
-module(peer_ctl).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {peer,
                version,
                send_pid,
                recv_pid,
                socket}).

-include("ecoin.hrl").

start_link(Peer) ->
    proc_lib:start_link(?MODULE, init, [Peer]).

init({IP, Port} = Peer) ->
    ok = proc_lib:init_ack({ok, self()}),

    {ok, ConnectionTimeout} = config:connection_timeout(),
    {ok, Socket} = ranch_tcp:connect(IP, Port, [], ConnectionTimeout),

    {ok, Version} = handshake(Socket, Peer),

    %% Create the send and receive processes
    {ok, SendPid} = peer_send:start_link(Socket),
    {ok, RecvPid} = peer_recv:start_link(Socket),

    State = #state{peer     = Peer,
                   version  = Version,
                   send_pid = SendPid,
                   recv_pid = RecvPid,
                   socket   = Socket},

    %% Enter gen_server loop
    gen_server:enter_loop(?MODULE, [], State).

handle_call(_, _, State) ->
    {stop, not_used, State}.

handle_cast(_, State) ->
    {stop, not_used, State}.

handle_info(_, State) ->
    {stop, not_used, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Initial handshake - outgoing
%%      - Send version
%%      - Receive remote version
%%        * Check if connection to self
%%      - Validate remote version
%%      - Send verack
handshake(Socket, Peer) ->
    Version = version:new(Peer),
    ok = protocol:send_message(Socket, Version),
    {ok, _, PeerVersion} = protocol:receive_message(Socket),

    true  = version:validate(PeerVersion),
    false = version:from_self(Version, PeerVersion),

    ok = protocol:send_message(Socket, #verack{}),
    {ok, _, #verack{}} = protocol:receive_message(Socket),
    {ok, PeerVersion}.
