-module(ecoin_blockchain).

-behaviour(gen_server).

-export([initialize/0,
         store/1,
         get/1,
         last_block/0,
         start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("ecoin.hrl").

-define(TAB, ?MODULE).
-define(N_FRAGMENTS, 16).

-record(state, {}).

initialize() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(block, [{attributes, record_info(fields, block)},
                                {disc_copies, [node()]},
                                {record_name, block},
                                {type, set}]),
    mnesia:create_table(tx, [{attributes, record_info(fields, tx)},
                             {record_name, tx},
                             {type, set},
                             {frag_properties, [{n_fragments, 16},
                                                {node_pool, [node()]},
                                                {n_disc_only_copies, 1}]}]),
    store(ecoin_block:genesis()).

store(Block = #block{txns = Txns}) ->
    Txns1 = array:map(fun(_Index, Tx) -> Tx#tx.hash end, Txns),
    BlockRow = Block#block{txns = Txns1},
    StoreBlock = fun() ->
        mnesia:write(BlockRow),
        lists:foreach(fun mnesia:write/1, array:to_list(Txns))
    end,
    mnesia:transaction(StoreBlock).

get(BlockNr) ->
    mnesia:transaction(fun () ->
        case mnesia:read(block, BlockNr) of
            []  -> {error, no_block};
            [B = #block{txns = Txns}] ->
                ReadTx = fun(_Index, Hash) -> mnesia:read(tx, Hash) end,
                Txns1 = array:map(ReadTx, Txns),
                B#block{txns = Txns1}
        end
    end).

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
