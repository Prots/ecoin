-module(inv).

-export([pack/1,
         unpack/2]).

-include("ecoin.hrl").

%% @doc Pack an inventory message (inv, getdata, notfound)
pack(#inv{inventory=Inventory}) ->
    pack_inv(Inventory);
pack(#getdata{data=Inventory}) ->
    pack_inv(Inventory);
pack(#notfound{data=Inventory}) ->
    pack_inv(Inventory).

%% @doc Unpack an inventory message (inv, getdata, notfound)
unpack(inv, Binary) ->
    #inv{inventory=unpack_inv(Binary)};
unpack(getdata, Binary) ->
    #getdata{data=unpack_inv(Binary)};
unpack(notfound, Binary) ->
    #notfound{data=unpack_inv(Binary)}.

%% @doc Pack an inventory
pack_inv(Inventory) ->
    Count = protocol:pack_var_uint(length(Inventory)),
    [Count | lists:map(fun pack_inv_vect/1, Inventory)].

%% @doc Unpack an inventory
unpack_inv(Binary0) ->
    {Count, Binary1} = protocol:unpack_var_uint(Binary0),
    unpack_inv_vects(Count, Binary1).

%% @doc Pack an inv_vect structure
pack_inv_vect({Type, Hash}) ->
   [<<(object_type_to_int(Type)):32/little>>, Hash]. 

%% @doc Unpack an inv_vect structure
unpack_inv_vect(<<Type:32/little, Hash:32/binary>>) ->
    {int_to_object_type(Type), Hash}.

%% @doc Unpack a given number of inv_vect structures
unpack_inv_vects(0, <<>>) ->
    [];
unpack_inv_vects(Count, <<Binary:36/binary, Rest/binary>>) ->
    [unpack_inv_vect(Binary) | unpack_inv_vects(Count-1, Rest)].

%% @doc Convert an object type to an integer
object_type_to_int(error)          -> 0;
object_type_to_int(transaction)    -> 1;
object_type_to_int(block)          -> 2;
object_type_to_int(filtered_block) -> 3.

%% @doc Convert an integer into an object type
int_to_object_type(0) -> error;
int_to_object_type(1) -> transaction;
int_to_object_type(2) -> block;
int_to_object_type(3) -> filtered_block.
