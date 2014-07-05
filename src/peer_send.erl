%% @doc Sending process
%%      This process takes care of the sending part of the socket
%%      The version handshake is done before the socket is handed
%%      of to this process.
%%      This should be the place to maybe buffer and rate limit
%%      the outgoing traffic to the peer
-module(peer_send).

-behaviour(gen_server).

-export([start_link/1,
         send/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% @doc Start the send process
start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

%% @doc Send a message to a peer
send(SendPid, Message) ->
    gen_server:cast(SendPid, {send, Message}).

init(Socket) ->
    {ok, Socket}.

handle_call(_, _, Socket) ->
    {stop, not_used, Socket}.

handle_cast({send, Message}, Socket) ->
    ok = message:send(Socket, Message),
    {noreply, Socket}.

handle_info(_, Socket) ->
    {stop, not_used, Socket}.

terminate(_, _) ->
    ok.

code_change(_, Socket, _) ->
    {ok, Socket}.
