%% @doc The peer manager
%%      This modules manages all connections to peers
-module(ecoin_peer_man).

-behaviour(gen_server).

-export([active_peers/0,
         inactive_peers/0]).

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

-record(state, {
          peers    :: [address()],
          outgoing :: uinteger(),
          limit    :: uinteger()
         }).

%% @doc Return a list of all active (connected) peers
-spec active_peers() -> active_peers().
active_peers() ->
    ets:tab2list(?CONN_TAB).

%% @doc Return a list of all inactive (not connected) peers
-spec inactive_peers() -> [address()].
inactive_peers() ->
    gen_server:call(?MODULE, inactive_peers).

-spec new_peers([#net_addr{}]) -> ok.
new_peers(Peers) ->
    gen_server:cast(?MODULE, {new_peers, Peers}).

%% @doc Let a peer process tell the manager that it has finished
%%      the handshake and is connected
-spec connected(#version{}) -> ok.
connected(Version) ->
    gen_server:cast(?MODULE, {connected, self(), Version}).

%% @doc Start the peer manager
-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, #state{}, 0}.
init([]) ->
    ConnectionLimit = ecoin_config:outgoing_limit(),
    PredefinedPeers = ecoin_config:predefined_peers(),
    Peers = PredefinedPeers ++ ecoin_util:dns_peers(),
    ToNetAddr = fun ({IP, Port}) -> #net_addr{ip = IP, port = Port} end,
    Peers1 =lists:map(ToNetAddr, Peers),

    ?CONN_TAB = ets:new(?CONN_TAB, [named_table]),

    State = #state{peers    = Peers1,
                   outgoing = 0,
                   limit    = ConnectionLimit},
    {ok, State, 0}.

handle_call(inactive_peers, _From, #state{peers = Peers} = State) ->
    {reply, Peers, State}.

handle_cast({new_peers, NewPeers}, State) ->
    lager:info("New peers: ~p", [NewPeers]),

    #state{peers    = Peers,
           outgoing = Outgoing,
           limit    = ConnectionLimit} = State,
    State1 = State#state{peers = Peers ++ NewPeers},
    case Outgoing < ConnectionLimit of
        true  -> {noreply, State1, 0};
        false -> {noreply, State1}
    end;
handle_cast({connected, Pid, Version}, State) ->
     case ets:lookup(?CONN_TAB, Pid) of
         [{Pid, connecting, _NetAddr, _Timestamp}] ->
             ets:insert(?CONN_TAB, {Pid, connected, Version, now()}),
             {noreply, State};
         [] ->
            {noreply, State}
     end.

handle_info({'DOWN', _Ref, process, Pid, Reason},
            #state{outgoing = Outgoing} = State) ->
    [{Pid, PeerState, PeerData, _Timestamp}] = ets:lookup(?CONN_TAB, Pid),
    Peer = case PeerData of
               #version{addr_from = NetAddr} -> NetAddr;
               #net_addr{}                   -> PeerData
           end,
    lager:info("Peer ~p (~p) [~p] died with reason: ~p.",
               [Peer, Pid, PeerState, Reason]),
    ets:delete(?CONN_TAB, Pid),
    {noreply, State#state{outgoing = Outgoing - 1}, 0};
handle_info(timeout, State) ->
    #state{peers    = Peers,
           outgoing = Outgoing,
           limit    = ConnectionLimit} = State,
    Needed = ConnectionLimit - Outgoing,
    {TryConnect, Peers1} = case Needed > length(Peers) of
                               true  -> {Peers, []};
                               false -> lists:split(Needed, Peers)
                          end,
    lists:foreach(fun new_peer/1, TryConnect),

    State1 = State#state{peers    = Peers1,
                         outgoing = Outgoing + length(TryConnect)},
    {noreply, State1}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%% @doc Spawn a new peer process, monitor it and add it to the
%%      connection table
-spec new_peer(#net_addr{}) -> true.
new_peer(Peer) ->
    {ok, Pid} = peer_sup:new_peer(Peer),
    monitor(process, Pid),
    ets:insert_new(?CONN_TAB, {Pid, connecting, Peer, now()}).
