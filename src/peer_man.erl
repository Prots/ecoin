%% @doc The peer manager
%%      This modules manages all connections to peers
-module(peer_man).

-behaviour(gen_server).

-export([connected_peers/0]).

-export([new_peers/1,
         connected/1,
         start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ecoin.hrl").

-define(CONN_TAB, connection_tab).

-record(state, {
          peers    :: [address()],
          outgoing :: non_neg_integer(),
          limit    :: pos_integer()
         }).

-type connected_peers() :: [connected_peer()].
-type connected_peer()  :: {pid(), connecting, address(), timestamp()} |
                           {pid(), connected, #version{}, timestamp()}.

%% @doc Return a list of all connected peers
-spec connected_peers() -> connected_peers().
connected_peers() ->
    ets:tab2list(?CONN_TAB).

%% @doc Supply the manager with new peers
-spec new_peers([address()]) -> ok.
new_peers(Peers) ->
    gen_server:cast(?MODULE, {new_peers, Peers}).

%% @doc Let a peer process tell the manager that it has finished
%%      the handshake and is connected
-spec connected(#version{}) -> ok.
connected(Version) ->
    gen_server:cast(?MODULE, {connected, self(), Version}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, #state{}, 0}.
init([]) ->
    ConnectionLimit = config:outgoing_limit(),
    PredefinedPeers = config:predefined_peers(),
    DNS             = config:dns(),

    ?CONN_TAB = ets:new(?CONN_TAB, [named_table]),

    lager:info("Starting the peer manager with connection limit: ~p",
               [ConnectionLimit]),
    lager:info("Predefined peers: ~p", [PredefinedPeers]),

    lager:info("Seeding with peers from given dns: ~p", [DNS]),
    DNSPeers = ecoin_util:get_peers_dns(DNS),

    State = #state{
               peers    = PredefinedPeers ++ DNSPeers,
               outgoing = 0,
               limit    = ConnectionLimit
              },
    {ok, State, 0}.

handle_call(_, _, State) ->
    {stop, not_used, State}.

handle_cast({new_peers, NewPeers}, State) ->
    lager:info("New peers: ~p", [NewPeers]),

    #state{
       peers    = Peers,
       outgoing = Outgoing, 
       limit    = ConnectionLimit
      } = State,
    State1 = State#state{peers = Peers ++ NewPeers},
    case Outgoing < ConnectionLimit of
        true -> 
            {noreply, State1, 0};
        false ->
            {noreply, State1}
    end;
handle_cast({connected, Pid, Version}, State) ->
    [{Pid, _Peer, connecting, undefined}] = ets:lookup(?CONN_TAB, Pid),
    ets:insert(?CONN_TAB, {Pid, connected, Version, now()}),
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    #state{outgoing = Outgoing} = State,
    [{Pid, PeerState, Version, _Timestamp}] = ets:lookup(?CONN_TAB, Pid),
    #version{addr_from = #net_addr{ip = IP, port = Port}} = Version,
    lager:info("Peer ~p (~p) [~p] died with reason: ~p.",
               [{IP, Port}, Pid, PeerState, Reason]),
    ets:delete(?CONN_TAB, Pid),
    {noreply, State#state{outgoing = Outgoing - 1}, 0};
handle_info(timeout, State) ->
    #state{
       peers    = Peers,
       outgoing = Outgoing,
       limit    = ConnectionLimit
      } = State,
    Needed = ConnectionLimit - Outgoing,
    {TryConnect, Peers1} = case Needed > length(Peers) of
                               true  -> {Peers, []};
                               false -> lists:split(Needed, Peers)
                          end,
    lists:foreach(fun new_peer/1, TryConnect),

    State1 = State#state{
               peers    = Peers1,
               outgoing = Outgoing + length(TryConnect)
              },
    {noreply, State1}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% @doc Spawn a new peer process, monitor it and add it to the
%%      connection table
-spec new_peer(address()) -> true.
new_peer(Peer) ->
    {ok, Pid} = peer_sup:new_peer(Peer),
    monitor(process, Pid),
    ets:insert_new(?CONN_TAB, {Pid, connecting, Peer, now()}).
