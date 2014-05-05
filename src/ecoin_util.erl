-module(ecoin_util).

-export([timestamp_to_int/1,
         int_to_timestamp/1,
         nounce/1,
         get_external_ip/0,
         get_peers_dns/1]).

%% @doc Convert a timestamp into a single big integer
timestamp_to_int({MSecs, Secs, _}) -> 
    MSecs*1000000+Secs.

%% @doc Convert a single big integer timestamp into an erlang timestamp
int_to_timestamp(Secs) ->
    {Secs div 1000000, Secs rem 1000000, 0}.

%% @doc Get a random nounce up to the given arg.
nounce(Max) ->
    random:uniform(Max).

%% @doc Get the external ip of this computer
get_external_ip() ->
    inets:start(),
    {ok, {_, _, Html}} = httpc:request("http://checkip.dyndns.org"),
    inets:stop(),
    [_,_|Rest] = lists:dropwhile(fun(X) -> X /= $: end, Html),
    IPString = lists:takewhile(fun(X) -> X/= $< end, Rest),
    inet:parse_ipv4_address(IPString).

%% @doc Retrive bootstrapping peers from dns servers
get_peers_dns(DNS) ->
    Success = fun({ok, _}) -> true;
                 (_)       -> false
              end,
    {ok, Port} = application:get_env(port),
    Result = [inet:getaddr(Host, inet) || Host <- DNS],
    Peers = [{IP, Port} || {ok, IP} <- lists:filter(Success, Result)],
    peer_man:new_peers(Peers).

