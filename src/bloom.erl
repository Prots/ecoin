%% @doc Implementation of the Bloom filter data structure. 
%%      Copied from the bloomerl project and refactored for use as the 
%%      bitcoin filter. Modified to use murmur3.
-module(bloom).
-export([new/1,
         new/2,
         new/3,
         is_element/2, 
         add_element/2]).

-record(bloom, {
          bitmap     :: binary(),
          nhashfuncs :: non_neg_integer(),
          ntweak     :: non_neg_integer() 
         }).

%% @doc Create a bloom filter of capacity N with 0.001 false positive rate.
-spec new(non_neg_integer()) -> #bloom{}.
new(N) -> new(N, 0.001).

%% @doc Creates a new Bloom filter, given a maximum number of keys and a
%%     false-positive error rate.
-spec new(non_neg_integer(), float()) -> #bloom{}.
new(N, E) when N > 0, is_float(E), E > 0, E =< 1 ->
    {Size, NHashFuncs} = calc_params(N, E),
    <<NTweak:32>> = crypto:rand_bytes(4),
    #bloom{bitmap     = <<0:Size>>,
           nhashfuncs = NHashFuncs, 
           ntweak     = NTweak}.

%% @doc Create a bloom filter from a given bitmap and hash functions
-spec new(binary(), non_neg_integer(), non_neg_integer()) -> #bloom{}.
new(Bitmap, NHashFuncs, NTweak) -> 
    #bloom{bitmap     = Bitmap,
           nhashfuncs = NHashFuncs,
           ntweak     = NTweak}.

%% @doc Determines if the key is (probably) an element of the filter.
-spec is_element(binary(), #bloom{}) -> boolean().
is_element(Key, Bloom) -> 
    is_element(Key, Bloom, calc_idxs(Key, Bloom)).
is_element(_, _, []) ->
    true;
is_element(Key, Bloom, [Idx | T]) ->
    ByteIdx = Idx div 8,
    <<_:ByteIdx/binary, Byte:8, _/binary>> = Bloom#bloom.bitmap,
    Mask = 1 bsl (Idx rem 8),
    case 0 =/= Byte band Mask of
        true  -> is_element(Key, Bloom, T);
        false -> false
    end.

%% @doc Adds the key to the filter.
-spec add_element(binary(), #bloom{}) -> #bloom{}.
add_element(Key, Bloom) ->
    Idxs = calc_idxs(Key, Bloom),
    Bloom#bloom{bitmap=set_bits(Bloom#bloom.bitmap, Idxs)}.

%% @doc Sets the given bits in the binary
-spec set_bits(binary(), list(non_neg_integer())) -> binary().
set_bits(Bin, []) -> 
    Bin;
set_bits(Bin, [Idx | Idxs]) ->
    ByteIdx = Idx div 8,
    <<Pre:ByteIdx/binary, Byte:8, Post/binary>> = Bin,
    Mask = 1 bsl (Idx rem 8),
    Byte0 = Byte bor Mask,
    set_bits(<<Pre/binary, Byte0:8, Post/binary>>, Idxs).

% Find the optimal bitmap size and number of hashes.
-spec calc_params(non_neg_integer(), float()) -> {non_neg_integer(), non_neg_integer()}.
calc_params(N, P) ->
    Size = round((-1 * math:pow(math:log(2), 2) * N * math:log(P)) / 8),
    NHashFuncs = round(Size * 8 / N * math:log(P)),
    {Size, NHashFuncs}.

calc_idxs(Key, #bloom{nhashfuncs=NHashFuncs, ntweak=NTweak}) ->
    [hash(HashNum, NTweak, Key) || HashNum <- lists:seq(0, NHashFuncs-1)].

hash(NHashNum, NTweak, Key) ->
    Seed = <<(NHashNum * 16#FBA4C795 + NTweak):32>>,
    murmerl:murmur3_32(Seed, Key).
