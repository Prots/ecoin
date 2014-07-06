%% @doc This module is the top-level supervisor for
%%      all connections that we initiate outwards.
-module(peer_sup).

-behaviour(supervisor).

-export([start_link/0,
         new_peer/1]).

-export([init/1]).

-include("ecoin.hrl").

%% @doc Start the supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start a new process that tries to connect to a remote peer
-spec new_peer(#net_addr{}) -> {ok, pid()}.
new_peer(Peer) ->
    supervisor:start_child(?MODULE, [Peer]).

init([]) ->
    PeerCtl = {peer_ctl,
               {peer_ctl, start_link, []},
               temporary, 5000, worker, [peer_ctl]},
    {ok, {{simple_one_for_one, 10, 10}, [PeerCtl]}}.
