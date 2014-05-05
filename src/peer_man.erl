-module(peer_man).

-behaviour(gen_server).

-export([new_peers/1,
         start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ecoin.hrl").

-define(CONN_TAB, connected_peers).
-define(PEER_TAB, peers).

-record(state, {peers          = [],
                outgoing       = [],
                num_outgoing   = 0,
                outgoing_limit = 10}).

new_peers(Peers) ->
    gen_server:cast(?MODULE, {new_peers, Peers}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, ConnectionLimit} = config:outgoing_limit(),
    {ok, PredefinedPeers} = config:predefined_peers(),
    {ok, DNS}             = config:dns(),
    lager:info("Starting the peer manager with connection limit: ~p",
               [ConnectionLimit]),

    lager:info("Predefined peers: ~p", [PredefinedPeers]),

    lager:info("Seeding with peers from given dns: ~p", [DNS]),
    spawn(ecoin_util, get_peers_dns, [DNS]),

    State = #state{peers          = PredefinedPeers,
                   outgoing_limit = ConnectionLimit},
    {ok, State, 0}.

handle_call(_, _, State) ->
    {stop, not_used, State}.

handle_cast({new_peers, NewPeers}, State0) ->
    #state{peers=Peers, num_outgoing=NumOutgoing, 
           outgoing_limit=OutgoingLimit} = State0,
    State1 = State0#state{peers=NewPeers ++ Peers},
    lager:info("New peers: ~p", [NewPeers]),
    case NumOutgoing < OutgoingLimit of
        true -> 
            {noreply, State1, 0};
        false ->
            {noreply, State1}
    end.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State0) ->
    #state{outgoing=Outgoing, num_outgoing=NumOutgoing} = State0,
    {Peer, Pid} = lists:keyfind(Pid, 2, Outgoing),
    lager:info("Outgoing peer ~p (~p) died with reason: ~p.", [Peer, Pid, Reason]),
    State1 = State0#state{outgoing=lists:keydelete(Pid, 2, Outgoing),
                          num_outgoing=NumOutgoing-1},
    {noreply, State1, 0};
handle_info(timeout, State0) ->
    #state{peers=Peers, outgoing=Outgoing, num_outgoing=NumOutgoing, 
           outgoing_limit=OutgoingLimit} = State0,
    NewNeeded = OutgoingLimit - NumOutgoing,
    {NewPeers, Peers0} = case NewNeeded > length(Peers) of
                             true ->
                                 {Peers, []};
                             false ->
                                 lists:split(NewNeeded, Peers)
                         end,

    NewOutgoing = [new_peer(Peer) || Peer <- NewPeers],
    State1 = State0#state{peers=Peers0,
                          outgoing=NewOutgoing ++ Outgoing,
                          num_outgoing=NumOutgoing+length(NewOutgoing)},
    {noreply, State1}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

new_peer(Peer) ->
    {ok, Pid} = peer_sup:new_peer(Peer),
    monitor(process, Pid),
    {Peer, Pid}.
