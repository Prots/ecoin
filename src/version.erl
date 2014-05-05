-module(version).

-export([new/1,
         validate/1,
         from_self/2,
         pack/1,
         unpack/1]).

-include("ecoin.hrl").

-define(MAX_NOUNCE, 16#ffffffffffffffff).

%% @doc Construct a version message with the current configuration
new({PeerIP, PeerPort}) ->
    {ok, IP}   = config:ip(),
    {ok, Port} = config:port(),
    Services   = config:services(),
    Receiving  = #net_addr{ip=PeerIP, port=PeerPort},
    Sending    = #net_addr{services=Services, ip=IP, port=Port},
    new(config:protocol_version(),
        Services,
        os:timestamp(),
        Receiving,
        Sending,
        ecoin_util:nounce(?MAX_NOUNCE),
        config:user_agent(),
        blockchain:last_block(),
        config:relay()).

%% @doc Pure function to construct a version message
new(ProtoVer, Services, Timestamp, Receiving, Sending,
    Nounce, UserAgent, StartHeight, Relay) ->
    #version{proto_ver    = ProtoVer,
             services     = Services,
             timestamp    = Timestamp,
             receiving    = Receiving,
             sending      = Sending,
             nounce       = Nounce,
             user_agent   = UserAgent,
             start_height = StartHeight,
             relay        = Relay}.

%% @doc Validate an incomping version message
%%      ALERT! Does nothing atm.
validate(_Version) ->
    Tests = [],
    Id = fun(X) -> X end,
    lists:all(Id, Tests).

%% @doc Check if a version message is from ourselves
from_self(#version{nounce=N1}, #version{nounce=N2}) ->
    N1 == N2.

%% @doc Pack a version record
pack(#version{proto_ver    = ProtoVer,
              services     = Services,
              timestamp    = Timestamp,
              sending      = Sending,
              receiving    = Receiving,
              nounce       = Nounce,
              user_agent   = UserAgent,
              start_height = StartHeight,
              relay        = Relay}) ->
    [<<ProtoVer:32/little>>,
     protocol:pack_services(Services),
     <<(ecoin_util:timestamp_to_int(Timestamp)):64/little>>,
     protocol:pack_net_addr(Sending),
     protocol:pack_net_addr(Receiving),
     <<Nounce:64/little>>,
     protocol:pack_varstr(UserAgent),
     <<StartHeight:32/little>>,
     pack_bool(Relay)].

%% @doc Unpack a version record
unpack(Binary0) ->
    <<ProtoVer:32/little,
      Services:8/binary,
      IntTimestamp:64/little,
      Receiving:26/binary,
      Sending:26/binary,
      Nounce:64/little,
      Rest0/binary>> = Binary0,
    {UserAgent, Binary1} = protocol:unpack_varstr(Rest0),
    <<StartHeight:32/little, Relay/binary>> = Binary1,
    
    #version{proto_ver    = ProtoVer,
             services     = protocol:unpack_services(Services),
             timestamp    = ecoin_util:int_to_timestamp(IntTimestamp),
             sending      = protocol:unpack_net_addr(Sending),
             receiving    = protocol:unpack_net_addr(Receiving),
             nounce       = Nounce,
             user_agent   = UserAgent,
             start_height = StartHeight,
             relay        = unpack_bool(Relay)}.

%% @doc Pack a boolean
pack_bool(true)  -> <<1:8/little>>;
pack_bool(false) -> <<0:8/little>>. 

%% @doc Unpack a boolean
unpack_bool(<<>>)           -> true;
unpack_bool(<<1:8/little>>) -> true;
unpack_bool(<<0:8/little>>) -> false.


