-module(peer_sup).

-behaviour(supervisor).

-export([start_link/0,
         new_peer/1]).


-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

new_peer(Peer) ->
    supervisor:start_child(?MODULE, [Peer]).

init([]) ->
    PeerCtl = {peer_ctl,
               {peer_ctl, start_link, []},
               temporary, 5000, worker, [peer_ctl]},
    {ok, {{simple_one_for_one, 10, 10}, [PeerCtl]}}.
