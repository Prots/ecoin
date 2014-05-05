-module(addr).

-export([pack/1,
         unpack/1]).

-include("ecoin.hrl").

%% @doc Pack an addr message
pack(#addr{addresses=Addrs}) -> 
    Count = protocol:pack_var_uint(array:size(Addrs)),
    [Count | lists:map(fun protocol:pack_net_addr/1,
                       array:to_list(Addrs))].

%% @doc Unpack an addr message
unpack(Binary0) ->
    {Count, Binary} = protocol:unpack_var_uint(Binary0),
    Addrs = array:from_list(unpack_net_addrs(Count, Binary)),
    #addr{addresses=Addrs}.

%% @doc Unpack a given number of net_addr structures
unpack_net_addrs(0, <<>>) ->
    [];
unpack_net_addrs(Count, <<Binary:30/binary, Rest/binary>>) ->
    [protocol:unpack_net_addr(Binary) | unpack_net_addrs(Count-1, Rest)].
