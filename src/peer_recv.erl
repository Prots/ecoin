%% @doc Reciving process
%%      This process just keep receiving messages from
%%      the peer until forever. Depending on the type
%%      of message we forward it to the correct place.
%%      This should be a good place to limit messages
%%      coming in from peer.
-module(peer_recv).

-export([start_link/1,
         init/1]).

-include("ecoin.hrl").

start_link(Socket) ->
    proc_lib:start_link(?MODULE, init, [Socket]).

init(Socket) ->
    ok = proc_lib:init_ack({ok, self()}),
    loop(Socket).

loop(Socket) ->
    {ok, _, Message} = protocol:receive_message(Socket),
    %% Message dispatch
    case Message of
        #addr{addr_list=Addresses} ->
            manager:new_peers(Addresses);
        #inv{inventory=Inventory} ->
            Inventory;
        _ -> io:format("Received message: ~p~n", [Message])
    end,
    loop(Socket).
