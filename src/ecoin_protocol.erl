-module(ecoin_protocol).

-export([empty_rest/1,
         encode_varuint/1,
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

%% @doc Returns only the object if rest is empty
-spec empty_rest({term(), <<>>})     -> term();
                ({term(), binary()}) -> {term(), binary()}.
empty_rest({Object, <<>>}) -> Object;
empty_rest({Object, Rest}) -> {Object, Rest}.

%% @doc Encode a list
-spec encode_list([T], fun ((T) -> iodata())) -> binary().
encode_list(List, Encode) ->
    encode_list(List, length(List), Encode).

-spec encode_list([T], uinteger(), fun ((T) -> iodata())) -> binary().
encode_list(List, Length, Encode) ->
    iolist_to_binary([encode_varuint(Length), lists:map(Encode, List)]).

%% @doc Decode a list
-spec decode_list(binary(), fun ((binary()) -> {T, binary()} | T)) ->
    {[T], binary()} | [T].
decode_list(Binary, Decode) ->
    {Length, Binary1} = decode_varuint(Binary),
    decode_list(Binary1, Decode, Length, []).

decode_list(Rest, _Decode, 0, Acc) ->
    empty_rest({lists:reverse(Acc), Rest});
decode_list(Binary, Decode, Count, Acc) ->
    {Elem, Binary1} = Decode(Binary),
    decode_list(Binary1, Decode, Count - 1, [Elem | Acc]).

%% @doc Encode an array
-spec encode_array(array(T), fun ((T) -> iodata())) -> binary().
encode_array(Array, Encode) ->
    encode_list(array:to_list(Array), array:size(Array), Encode).

%% @doc Decode an array
-spec decode_array(binary(), fun ((binary()) -> {T, binary()})) ->
    {array(T), binary()}.
decode_array(Binary, Decode) ->
    case decode_list(Binary, Decode) of
        {List, Rest} -> {array:from_list(List), Rest};
        List when is_list(List) -> array:from_list(List)
    end.

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
-spec encode_varuint(uinteger()) -> binary().
encode_varuint(Integer) when Integer < 16#fd ->
    Integer;
encode_varuint(Integer) when Integer =< 16#ffff ->
    <<16#fd, Integer:16/little>>;
encode_varuint(Integer) when Integer =< 16#ffffffff ->
    <<16#fe, Integer:32/little>>;
encode_varuint(Integer) when Integer  < 16#ffffffffffffffff ->
    <<16#ff, Integer:64/little>>.

%% @doc Decode a variable length integer
-spec decode_varuint(binary()) -> {uinteger(), binary()} | uinteger().
decode_varuint(<<Integer, Binary/binary>>) when Integer < 16#fd ->
    empty_rest({Integer, Binary});
decode_varuint(<<16#fd, Integer:16/little, Binary/binary>>) ->
    empty_rest({Integer, Binary});
decode_varuint(<<16#fe, Integer:32/little, Binary/binary>>) ->
    empty_rest({Integer, Binary});
decode_varuint(<<16#ff, Integer:64/little, Binary/binary>>) ->
    empty_rest({Integer, Binary}).

%% @doc Encode a variable length binary
-spec encode_varbin(binary()) -> binary().
encode_varbin(Binary) ->
    <<(encode_varuint(byte_size(Binary)))/binary, Binary/binary>>.

%% @doc Decode a variable length binary
-spec decode_varbin(binary()) -> {binary(), binary()} | binary().
decode_varbin(Binary) ->
    {Length, Binary1} = decode_varuint(Binary),
    empty_rest(split_binary(Binary1, Length)).
