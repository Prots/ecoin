-module(blockchain).

-behaviour(gen_server).

-export([last_block/0,
         start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(TAB, ?MODULE).

-record(state, {}).

last_block() ->
    [{last_block, BlockNum}] = ets:lookup(?TAB, last_block),
    BlockNum.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?TAB, [named_table]),
    ets:insert_new(?TAB, {last_block, 0}),
    {ok, #state{}}.

handle_call(_, _, State) ->
    {stop, not_used, State}.

handle_cast(_, State) ->
    {stop, not_used, State}.

handle_info(_, State) ->
    {stop, not_used, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.
