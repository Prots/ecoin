-module(protocol).

-export([pack_net_addr/1,
         unpack_net_addr/1,
         pack_var_uint/1,
         unpack_var_uint/1,
         pack_varstr/1,
         unpack_varstr/1,
         pack_services/1,
         unpack_services/1]).

-include("ecoin.hrl").

%% @doc Pack a net_addr structures
pack_net_addr(NetAddr) ->
    #net_addr{time=Time0, services=Services, ip=IP, port=Port} = NetAddr,
    Time = case Time0 of
               undefined -> <<>>;
               _         -> <<Time0:32/little>>
           end,
    [Time,
     pack_services(Services),
     pack_ip(IP),
     <<Port:16>>].

%% @doc Unpack a net_addr structure
%%      Pattern match on 26 bytes, if fail fall through and
%%      match on Time and then match the rest.
unpack_net_addr(<<Services:8/binary, IP:16/binary, Port:16>>) ->
    #net_addr{services = unpack_services(Services),
              ip       = unpack_ip(IP),
              port     = Port};
unpack_net_addr(<<Time:32/little, Binary/binary>>) ->
    NetAddr = unpack_net_addr(Binary),
    NetAddr#net_addr{time=Time}.

%% @doc Serialize an ipv4 address to ipv6 representation
pack_ip({I1,I2,I3,I4}) ->
    <<16#ffff:96, I1, I2, I3, I4>>.

%% @doc Unpack an IPv4 mapped IPv6 address, return an IPv4 address.
unpack_ip(<<16#ffff:96, I1, I2, I3, I4>>) ->
    {I1, I2, I3, I4}.

%% @doc Pack a variable length integer
pack_var_uint(Integer) when Integer < 16#fd -> 
    [Integer];
pack_var_uint(Integer) when Integer =< 16#ffff ->
    [16#fd, <<Integer:16/little>>];
pack_var_uint(Integer) when Integer =< 16#ffffffff ->
    [16#fe, <<Integer:32/little>>];
pack_var_uint(Integer) when Integer  < 16#ffffffffffffffff ->
    [16#ff, <<Integer:64/little>>].

%% @doc Unpack a variable length integer
unpack_var_uint(<<Integer, Binary/binary>>) when Integer < 16#fd -> 
    {Integer, Binary};
unpack_var_uint(<<16#fd, Integer:16/little, Binary/binary>>) -> 
    {Integer, Binary};
unpack_var_uint(<<16#fe, Integer:32/little, Binary/binary>>) -> 
    {Integer, Binary};
unpack_var_uint(<<16#ff, Integer:64/little, Binary/binary>>) ->
    {Integer, Binary}.

%% @doc Pack a variable length string
pack_varstr(String) ->
    [pack_var_uint(length(String)),
     list_to_binary(String)].

%% @doc Unpack a variable length string
unpack_varstr(Binary0) ->
    {Length, Binary1} = unpack_var_uint(Binary0),
    <<String:Length/binary, Binary2/binary>> = Binary1,
    {binary_to_list(String), Binary2}.

%% @doc Pack a list of services
pack_services(Services) ->
    pack_services(Services, 0).

%% @doc Pack the services
%%      Start out with a mask of 0
%%      For each services, find out it's serializable value
%%      and xor it into the mask.
pack_services([], Mask)     ->
    <<Mask:64/little>>;
pack_services([Service|Services], Mask) -> 
    pack_services(Services, Mask bxor service_to_int(Service)).

%% @doc Unpack a service bitfield into a list of services
unpack_services(<<Mask:64/little>>) ->
    [Service || Service <- all_services(), 
                in_mask(service_to_int(Service), Mask)].

%% @doc All defined services
all_services() -> [node_network].

%% @doc Turn a service into an integer to be included
%%      into the services bitfield.
service_to_int(node_network) -> 1.

%% @doc Check if an integer is in a mask
in_mask(I, M) -> M /= (I bxor M).
