-module(ecoin_util).

-export([timestamp_to_integer/1,
         integer_to_timestamp/1,
         nounce/1,
         in_mask/2,
         get_external_ip/0,
         dns_peers/0,
         dns_peers/2]).

-type address() :: {inet:ip_address(), inet:port_number()}.
-type hostaddr() :: inet:hostname() | inet:ip_address().

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
-spec get_external_ip() -> inet:ip_address().
get_external_ip() ->
    inets:start(),
    {ok, {_, _, Html}} = httpc:request("http://checkip.dyndns.org"),
    inets:stop(),
    [_,_|Rest] = lists:dropwhile(fun(X) -> X /= $: end, Html),
    IPString = lists:takewhile(fun(X) -> X/= $< end, Rest),
    inet:parse_ipv4_address(IPString).

%% @doc Retrive bootstrapping peers from dns server(s)
%%      Grab the dns servers and how many entries we want
%%      from the configuration.
-spec dns_peers() -> [address()].
dns_peers() ->
    dns_peers(config:dns(), config:dns_limit()).

%% @doc Same as above but with explicit parameters.
-spec dns_peers([hostaddr()] | hostaddr(), non_neg_integer()) -> [address()].
dns_peers(DNSs, DNSLimit) when is_list(DNSs) ->
        Port = config:default_port(),
        AccDNS = fun (_DNS, {Acc, 0}) ->
                         {Acc, 0}; 
                     (DNS, {Acc, Left}) ->
                         try
                             {ok, IPs} = inet:getaddrs(DNS, inet),
                             Length = length(IPs),
                             ToPeer = fun (IP) -> {IP, Port} end,
                             case Length > Left of
                                 true ->
                                     {IPs1, _} = lists:split(Left, IPs),
                                     Peers = lists:map(ToPeer, IPs1),
                                     {Peers ++ Acc, 0};
                                 false ->
                                     Peers = lists:map(ToPeer, IPs),
                                     {Peers ++ Acc, Left - Length}
                             end
                         catch error:_ -> {Acc, Left}
                         end
                 end,
        {DNSPeers, 0} = lists:foldl(AccDNS, {[], DNSLimit}, DNSs),
        DNSPeers;
dns_peers(DNS, DNSLimit) ->
    dns_peers([DNS], DNSLimit).
