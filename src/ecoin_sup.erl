-module(ecoin_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    PeerSup = {ecoin_peer_sup,
               {ecoin_peer_sup, start_link, []},
               permanent, 5000, supervisor, [ecoin_peer_sup]},
    PeerMan = {ecoin_peer_man,
               {ecoin_peer_man, start_link, []},
               permanent, 5000, worker, [ecoin_peer_man]},
    BlockChain = {ecoin_blockchain,
                  {ecoin_blockchain, start_link, []},
                  permanent, 5000, worker, [ecoin_blockchain]},

    Specs = [PeerSup, PeerMan, BlockChain],

    {ok, {{one_for_one, 10, 10}, Specs}}.
