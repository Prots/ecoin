-module(ecoin_util).

-export([timestamp_to_integer/1,
         integer_to_timestamp/1,
         nounce/1,
         in_mask/2,
         get_external_ip/0,
         get_peers_dns/1]).

%% @doc Convert a timestamp into a single big integer
-spec timestamp_to_integer(erlang:timestamp()) -> non_neg_integer().
timestamp_to_integer({MSecs, Secs, _}) -> 
    MSecs*1000000+Secs.

%% @doc Convert a single big integer timestamp into an erlang timestamp
-spec integer_to_timestamp(non_neg_integer()) -> erlang:timestamp().
integer_to_timestamp(Secs) ->
    {Secs div 1000000, Secs rem 1000000, 0}.

%% @doc Get a random nounce of given maximum length in bytes
-spec nounce(non_neg_integer()) -> non_neg_integer().
nounce(Bytes) ->
    binary:decode_unsigned(crypto:rand_bytes(Bytes), little).

%% @doc Check if an integer is in a mask
-spec in_mask(non_neg_integer(), non_neg_integer()) -> boolean().
in_mask(I, M) -> M /= (I bxor M).

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

