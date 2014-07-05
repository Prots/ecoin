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

-record(state, {
          send_pid :: pid(),
          socket   :: inet:socket()
         }).

-include("ecoin.hrl").


%% @doc Start a new outgoing peer process
-spec start_link(address()) -> {ok, pid()}.
start_link(Peer) ->
    proc_lib:start_link(?MODULE, init, [Peer]).

-spec init(address()) -> {ok, #state{}}.
init({IP, Port} = Peer) ->
    ok = proc_lib:init_ack({ok, self()}),

    {ok, Socket} = ranch_tcp:connect(IP, Port, [], config:connection_timeout()),

    %% Do the handshake and keep the peers version
    Version = handshake(Socket, Peer),

    %% Inform the peer manager about the peers version and
    %% that the peer is connected
    peer_man:connected(Version),

    %% Create the send and receive processes
    {ok, SendPid} = peer_send:start_link(Socket),
    {ok, _} = peer_recv:start_link(Socket, self()),

    State = #state{
               send_pid = SendPid,
               socket   = Socket
              },

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
%%      - Send our version
%%      - Receive peer version
%%        * Check if connection to self
%%      - TODO: Validate peer version
%%      - Send and receive veracks
-spec handshake(socket(), address()) -> #version{}.
handshake(Socket, Peer) ->
    %% Get which network we are on
    Network   = config:network(),

    %% Create and send our version
    MyVersion = my_version(Peer),
    ok = message:send(Socket, MyVersion),

    %% Receive the peers version
    #message{
       network = Network,
       command = version,
       payload = Version
      } = message:recv(Socket),

    %% Assert that we do not connect to ourselves
    (MyVersion#version.nounce == Version#version.nounce) andalso error(connect_to_self),

    %% Send and receive veracks
    ok = message:send(Socket, verack),
    #message{
       network = Network,
       command = verack
      } = message:recv(Socket),

    %% Return the peers version
    Version.

%% @doc Construct the version message to send to a peer
%%      NOTE: Placed here because I only want pure code in version.erl
-spec my_version(address()) -> #version{}.
my_version({IP, Port}) ->
    Services = config:services(),
    #version{
       version   = config:protocol_version(),
       services  = Services,
       timestamp = now(),
       addr_recv = #net_addr{
                      ip       = IP,
                      port     = Port,
                      services = []
                     },
       addr_from = #net_addr{
                      ip       = config:get_ip(),
                      port     = config:port(),
                      services = Services
                     },
       nounce       = ecoin_util:nounce(4),
       user_agent   = config:user_agent(),
       start_height = blockchain:last_block(),
       relay        = config:relay()
      }.
