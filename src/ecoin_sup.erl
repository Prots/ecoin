-module(ecoin_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    PeerSup = {peer_sup,
               {peer_sup, start_link, []},
               permanent, 5000, supervisor, [peer_sup]},
    PeerMan = {peer_man,
               {peer_man, start_link, []},
               permanent, 5000, worker, [peer_man]},
    BlockChain = {blockchain,
                  {blockchain, start_link, []},
                  permanent, 5000, worker, [blockchain]},

    Specs = [PeerSup, PeerMan, BlockChain],

    {ok, {{one_for_one, 10, 10}, Specs}}.
