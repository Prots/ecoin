%% @doc Reciving process
%%      This process just keep receiving messages from
%%      the peer until forever. Depending on the type
%%      of message we forward it to the correct place.
%%      This should be a good place to limit messages
%%      coming in from peer.
-module(ecoin_peer_recv).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ecoin.hrl").

-record(state, {
          ctl_pid  :: pid(),
          send_pid :: pid(),
          socket   :: socket()
         }).

%% @doc Start the receiving peer process
-spec start_link(socket(), pid(), pid()) -> {ok, pid()}.
start_link(Socket, ControlPid, SendPid) ->
    gen_server:start_link(?MODULE, [Socket, ControlPid, SendPid], []).

init([Socket, ControlPid, SendPid]) ->
    State = #state{ctl_pid  = ControlPid,
                   send_pid = SendPid,
                   socket   = Socket},
    {ok, State, 0}.

handle_call(_Msg, _From, State) ->
    {stop, not_used, State}.

handle_cast(_Msg, State) ->
    {stop, not_used, State}.

handle_info(timeout, #state{socket = Socket} = State) ->
    #message{command = Command,
             payload = Payload} = ecoin_message:recv(Socket),
    Payload1 = case Payload == undefined of
                   true  -> Command;
                   false -> Payload
               end,
    handle(Payload1, State),
    {noreply, State, 0}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @doc Handle an incoming message
-spec handle(message_type(), #state{}) -> ok.
handle(#addr{addr_list = AddrList}, _State) ->
    ecoin_peer_man:new_peers(AddrList);
handle(#inv{}, _State) ->
    ok;
handle(#getdata{}, _State) ->
    ok;
handle(#notfound{}, _State) ->
    ok;
handle(#getblocks{}, _State) ->
    ok;
handle(#getheaders{}, _State) ->
    ok;
handle(#tx{}, _State) ->
    ok;
handle(#block{}, _State) ->
    ok;
handle(#headers{}, _State) ->
    ok;
handle(getaddr, #state{ctl_pid  = ControlPid, send_pid = SendPid}) ->
    ecoin_peer_send:send(SendPid, addr:new(ControlPid));
handle(mempool, _State) ->
    ok;
handle(#ping{nounce = Nounce}, #state{send_pid = SendPid}) ->
    ecoin_peer_send:send(SendPid, #pong{nounce = Nounce});
handle(#pong{}, _State) ->
    ok;
handle(#reject{}, _State) ->
    ok;
handle(#filterload{}, _State) ->
    ok;
handle(#filteradd{}, _State) ->
    ok;
handle(filterclear, _State) ->
    ok;
handle(#merkleblock{}, _State) ->
    ok;
handle(#alert{}, _State) ->
    ok.
