-module(protocol).

-export([encode_varuint/1,
         decode_varuint/1,
         encode_varbin/1,
         decode_varbin/1,
         encode_list/2,
         decode_list/2,
         encode_array/2,
         decode_array/2,
         split/2,
         split_decode/3]).

-include("ecoin.hrl").

%% @doc Encode a list
-spec encode_list([T], fun ((T) -> iodata())) -> iodata().
encode_list(List, Encode) ->
    [
     encode_varuint(length(List)),
     lists:map(Encode, List)
    ].

%% @doc Decode a list
-spec decode_list(binary(), fun ((binary()) -> {T, binary()})) ->
    {[T], binary()}.
decode_list(Binary, Decode) ->
    {Length, Binary1} = decode_varuint(Binary),
    decode_list(Binary1, Decode, Length, []).

decode_list(Rest, _Decode, 0, Acc) ->
    {lists:reverse(Acc), Rest};
decode_list(Binary, Decode, Count, Acc) ->
    {Elem, Binary1} = Decode(Binary),
    decode_list(Binary1, Decode, Count - 1, [Elem | Acc]).

%% @doc Encode an array
-spec encode_array(array(T), fun ((T) -> iodata())) -> iodata().
encode_array(Array, Encode) ->
    encode_list([Elem || {_, Elem} <- array:to_list(Array)], Encode).

%% @doc Decode an array
-spec decode_array(binary(), fun ((binary()) -> {T, binary()})) ->
    {array(T), binary()}.
decode_array(Binary, Decode) ->
    {List, Rest} = decode_list(Binary, Decode),
    {array:from_list(List), Rest}.

%% @doc Split a binary into fixed sized chunks
-spec split(binary(), pos_integer()) -> [binary()].
split(<<>>,  _ChunkSize) -> [];
split(Binary, ChunkSize) ->
    {Elem, Rest} = split_binary(Binary, ChunkSize),
    [Elem | split(Rest, ChunkSize)].

%% @doc Split a binary into fixed sized chunks and decode them
%%      into a list.
-spec split_decode(binary(), pos_integer(), fun ((binary()) -> T)) -> [T].
split_decode(<<>>,  _ChunkSize, _Decode) -> [];
split_decode(Binary, ChunkSize,  Decode) ->
    {Elem, Rest} = split_binary(Binary, ChunkSize),
    [Decode(Elem) | split_decode(ChunkSize, Rest, Decode)].

%% @doc Encode a variable length integer
-spec encode_varuint(uinteger()) -> iodata().
encode_varuint(Integer) when Integer < 16#fd -> 
    Integer;
encode_varuint(Integer) when Integer =< 16#ffff ->
    [16#fd, <<Integer:16/little>>];
encode_varuint(Integer) when Integer =< 16#ffffffff ->
    [16#fe, <<Integer:32/little>>];
encode_varuint(Integer) when Integer  < 16#ffffffffffffffff ->
    [16#ff, <<Integer:64/little>>].

%% @doc Decode a variable length integer
-spec decode_varuint(binary()) -> {uinteger(), binary()}.
decode_varuint(<<Integer, Binary/binary>>) when Integer < 16#fd -> 
    {Integer, Binary};
decode_varuint(<<16#fd, Integer:16/little, Binary/binary>>) -> 
    {Integer, Binary};
decode_varuint(<<16#fe, Integer:32/little, Binary/binary>>) -> 
    {Integer, Binary};
decode_varuint(<<16#ff, Integer:64/little, Binary/binary>>) ->
    {Integer, Binary}.

%% @doc Encode a variable length binary
-spec encode_varbin(binary()) -> iodata().
encode_varbin(Binary) -> [encode_varuint(length(Binary)), Binary].

%% @doc Decode a variable length binary
-spec decode_varbin(binary()) -> {binary(), binary()}.
decode_varbin(Binary) ->
    {Length, Binary1} = decode_varuint(Binary),
    split_binary(Binary1, Length).
