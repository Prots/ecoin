-module(stack).

-export([new/0, from_list/1, to_list/1, sz/1]).
-export([push/2, push_n/2]).
-export([pop/1, pop_n/2, pop_ix/2, pop_part/3]).
-export([peek/1, peek_n/2, peek_ix/2, peek_part/3]).

-type item() :: any().
-type items() :: list(any()).

-type ix() :: non_neg_integer().
-type len() :: non_neg_integer().

-record(stack, {
          stack = [] :: items(),
          sz  = 0  :: len()
         }).

-opaque stack() :: #stack{}.
-export_type([stack/0]).

%%-----------------------------------------------------------------------------
%% Creation, destruction and information
%%-----------------------------------------------------------------------------

-spec new() -> stack().
new() -> #stack{}.

-spec from_list(list()) -> stack().
from_list(List) ->
    #stack{stack=List, sz=length(List)}.

-spec to_list(stack()) -> list().
to_list(#stack{stack=Stack}) -> Stack.

-spec sz(stack()) -> non_neg_integer().
sz(#stack{sz=Sz}) -> Sz.

%%-----------------------------------------------------------------------------
%% Push items onto stack
%%-----------------------------------------------------------------------------

-spec push(stack(), item()) -> stack().
push(Stack, Item) -> push_n(Stack, [Item]).

-spec push_n(stack(), items()) -> stack().
push_n(#stack{stack=Stack, sz=sz}, Items) ->
    #stack{stack=Items ++ Stack, sz=sz+length(Items)}.

%%-----------------------------------------------------------------------------
%% Pop items from stack (remove and return)
%%-----------------------------------------------------------------------------

-spec pop_part(stack(), ix(), len()) -> {items(), stack()}.
pop_part(#stack{stack=Stack, sz=Sz}, Start, Len) ->
    try
        {Front, Rest0} = lists:split(Len, Stack),
        {Rest1, Part} = lists:split(Start, Front),
        {Part, #stack{stack=Rest1++Rest0, sz=Sz-Len}}
    catch
        error:badarg -> error(stack_empty)
    end.

-spec pop(stack()) -> {item(), stack()}.
pop(Stack0) ->
    {[H], Stack1} = pop_part(Stack0, 0, 1),
    {H, Stack1}.

-spec pop_n(stack(), len()) -> {items(), stack()}.
pop_n(Stack, N) ->
    pop_part(Stack, 0, N).

-spec pop_ix(stack, ix()) -> {item(), stack()}.
pop_ix(Stack0, Ix) ->
    {[Item], Stack1} = pop_part(Stack0, Ix, 1),
    {Item, Stack1}.

%%-----------------------------------------------------------------------------
%% Peek items from the stack (return but don't remove)
%%-----------------------------------------------------------------------------

-spec peek_part(stack(), ix(), len()) -> items().
peek_part(Stack, Start, Len) -> element(1, pop_part(Stack, Start, Len)).

-spec peek(stack()) -> item().
peek(Stack) -> hd(peek_part(Stack, 0, 1)).

-spec peek_n(stack(), len()) -> items().
peek_n(Stack, N) -> peek_part(Stack, 0, N).

-spec peek_ix(stack(), ix()) -> item().
peek_ix(Stack, Ix) -> hd(peek_part(Stack, Ix, 1)).

%%-----------------------------------------------------------------------------
%% Testing
%%-----------------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

new_stack_test_() ->
     [?_assertEqual(0, sz(new())),
     ?_assertEqual([], to_list(new()))].

from_to_list_test_() ->
    [?_assertEqual([], to_list(from_list([]))),
     ?_assertEqual(lists:seq(1,10), to_list(from_list(lists:seq(1,10)))),
     ?_assertEqual(1, peek(from_list(lists:seq(1,10))))].

filled_stack_test_() ->
    Items = lists:seq(1,10),
    Stack = from_list(Items),
    [?_assertEqual({1, from_list(lists:seq(2,10))}, pop(Stack)),
     ?_assertEqual(10, sz(Stack))].

-endif.
