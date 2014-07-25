-module(ecoin_util).

-export([ts_to_int/1,
         int_to_ts/1,
         nounce/1,
         in_mask/2,
         get_external_ip/0,
         dns_peers/0,
         dns_peers/2,
         bin_to_hexstr/1,
         hexstr_to_bin/1,
         ts_to_bin/1]).

-type address() :: {inet:ip_address(), inet:port_number()}.
-type hostaddr() :: inet:hostname() | inet:ip_address().

%% @doc Convert a timestamp into a single big integer
-spec ts_to_int(erlang:timestamp()) -> non_neg_integer().
ts_to_int({MSecs, Secs, _}) ->
    MSecs*1000000+Secs.

%% @doc Convert a single big integer timestamp into an erlang timestamp
-spec int_to_ts(non_neg_integer()) -> erlang:timestamp().
int_to_ts(Secs) ->
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

%% @doc Turn a binary into a hexadecimal string
-spec bin_to_hexstr(binary()) -> string().
bin_to_hexstr(Bin) ->
  list_to_binary(lists:flatten([io_lib:format("~2.16.0B", [X]) ||
                                X <- binary_to_list(Bin)])).

%% @doc Turn a hexadecimal string into a binary
-spec hexstr_to_bin(binary()) -> binary().
hexstr_to_bin(HexStr) ->
  hexstr_to_bin(binary_to_list(HexStr), []).

hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).

ts_to_bin(Timestamp) ->
    {Date, Time} = calendar:now_to_universal_time(Timestamp),
    {Year, Month, Day} = Date,
    {Hour ,Minute, Second} = Time,
    <<
      (integer_to_binary(Year))/binary, "-",
      (integer_to_binary(Month))/binary, "-",
      (integer_to_binary(Day))/binary, " ",
      (integer_to_binary(Hour))/binary, ":",
      (integer_to_binary(Minute))/binary, ":",
      (integer_to_binary(Second))/binary
    >>.

