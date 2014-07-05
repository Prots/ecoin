-module(ecoin_stats).

-behaviour(gen_server).

-export([incoming/3,
         outgoing/3,
         start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(STATS_TAB, ecoin_statistics_tab).

incoming(CtlPid, Command, Length) ->
    gen_server:cast(?MODULE, {incoming, CtlPid, Command, Length}).

outgoing(CtlPid, Command, Length) ->
    gen_server:cast(?MODULE, {outgoing, CtlPid, Command, Length}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ?STATS_TAB = ets:new(?STATS_TAB, [named_table]),
    Counters = [
                {incoming, 0},
                {outgoing, 0}
               ],
    true = ets:insert_new(?STATS_TAB, Counters),
    {ok, nostate}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
