%% @doc Implements hierarchial deterministic wallets (BIPS 0032)
-module(hdw).

-export([ckd_priv/2,
         ckd_pub/2,
         n/1,
         generate_master_key/1,
         pack_extended_key/1,
         unpack_extended_key/1
         ]).

-define(SEED_BYTES, 32).
-define(SEED, <<"Bitcoin seed">>).

-define(IS_HARDENED(Ix), Ix >= 16#80000000).
-define(IX_IN_RANGE(Ix), Ix >= 0 andalso Ix < 16#0100000000).

-type private_key() :: non_neg_integer().
-type public_key() ::  {odd | even, non_neg_integer()}.
-type key_type() :: public | private.
-type network() :: main | testnet.

-record(extended_key, {
          network      = main    :: network(),
          type         = private :: key_type(),
          depth        = 0       :: non_neg_integer(),
          fingerprint  = 0       :: binary(),
          child_number = 0       :: non_neg_integer(),
          chain_code             :: binary(),
          key                    :: private_key() | public_key()
         }).

generate_master_key(Network) ->
    {MasterKey, ChainCode} = generate_master_key_(),
    #extended_key{network    = Network,
                  chain_code = ChainCode,
                  key        = MasterKey}.

%% @doc Generate a master key and chain code
generate_master_key_() ->
    {_, _, _, N, _} = curve_params(),
    S = crypto:rand_bytes(?SEED_BYTES),
    I = crypto:hmac(sha512, ?SEED, S),
    <<MasterKey:256, ChainCode:32/binary>> = I,
    case MasterKey == 0 orelse MasterKey >= N of
        true  -> generate_master_key_();
        false -> {MasterKey, ChainCode}
    end.

%% @doc Private parent key -> private child key
ckd_priv(#extended_key{network=Network, type=private, depth=Depth,
                       chain_code=ChainCode, key=Key},
         Ix) when ?IX_IN_RANGE(Ix) ->
    Fingerprint = fingerprint(compute_public_key(Key)),
    {PrivateKeyChild, ChainCodeChild} = ckd_priv(Key, ChainCode, Ix),
    #extended_key{network=Network, type=private, depth=Depth+1, 
                  fingerprint=Fingerprint, child_number=Ix,
                  chain_code=ChainCodeChild, key=PrivateKeyChild}.

%% @doc Public parent key -> public child key
ckd_pub(_, Ix) when ?IS_HARDENED(Ix) ->
    {error, hardened};
ckd_pub(#extended_key{network=Network, type=public, depth=Depth,
                      fingerprint=Fingerprint, chain_code=ChainCode, key=Key},
        Ix) when ?IX_IN_RANGE(Ix) ->
    Fingerprint = fingerprint(Key),
    {PublicKeyChild, ChainCodeChild} = ckd_pub(Key, ChainCode, Ix),
    #extended_key{network=Network, type=public, depth=Depth+1,
                  fingerprint=Fingerprint, child_number=Ix,
                  chain_code=ChainCodeChild, key=PublicKeyChild}.

%% @doc Private parent key -> Extended public key
n(#extended_key{type=private, key=Key} = ExtKey) ->
    PublicKey = compute_public_key(Key),
    ExtKey#extended_key{type=public, key=PublicKey}.

ckd_priv(Key, Chain, Ix)  ->
    Data = case ?IS_HARDENED(Ix) of
               true  -> <<16#00, Key:256, Ix:32>>;
               false -> <<(pack_point(point(Key)))/binary, Ix:32>>
           end,

    <<KeyChild0:256, ChainChild:32/binary>> = crypto:hmac(sha512, Chain, Data),

    {_, _, _, <<N:256>>, _} = curve_params(),
    KeyChild = (KeyChild0 + Key) rem N,
    {KeyChild, ChainChild}.

ckd_pub(KeyParent, ChainParent, Ix) ->
    I = crypto:hmac(sha512, ChainParent,
                    <<(pack_point(KeyParent))/binary, Ix:32>>),
    <<PointChild:256, ChainChild:32/binary>> = I,
    KeyChild = point(PointChild),
    {KeyChild, ChainChild}.

pack_extended_key(ExtKey) when is_record(ExtKey, extended_key) ->
    #extended_key{network=Network, type=KeyType, depth=Depth,
                  fingerprint=Fingerprint, child_number=ChildNumber,
                  chain_code=ChainCode, key=Key} = ExtKey,
    <<(pack_version(Network, KeyType)):4/binary,
       Depth:8,
       Fingerprint:32,
       ChildNumber:32,
       ChainCode:32/binary,
       (pack_key(KeyType, Key)):33/binary>>.

unpack_extended_key(Binary) when byte_size(Binary) == 78 ->
    <<Version:4/binary, Depth:8, Fingerprint:32, ChildNumber:32,
      ChainCode:32/binary, Key0:33/binary>> = Binary,
    {Network, Type} = unpack_version(Version),
    {Type, Key} = unpack_key(Key0),
    #extended_key{network=Network,
                  type=Type,
                  depth=Depth,
                  fingerprint=Fingerprint,
                  child_number=ChildNumber,
                  chain_code=ChainCode,
                  key=Key}.

unpack_key(<<16#00, PrivateKey:256>>) ->
    {private, PrivateKey};
unpack_key(PublicKey) ->
    {public, unpack_point(PublicKey)}.

pack_key(private, Key) ->
    <<16#00, Key:256>>;
pack_key(public, Key) ->
    pack_point(Key).

unpack_version(<<16#0488B21E:32>>) -> {main, public};
unpack_version(<<16#0488ADE4:32>>) -> {main, private};
unpack_version(<<16#043587CF:32>>) -> {testnet, public};
unpack_version(<<16#04358394:32>>) -> {testnet, private}.

pack_version(main, public)     -> <<16#0488B21E:32>>;
pack_version(main, private)    -> <<16#0488ADE4:32>>;
pack_version(testnet, public)  -> <<16#043587CF:32>>;
pack_version(testnet, private) -> <<16#04358394:32>>.

compute_public_key(PrivateKey) ->
    point(PrivateKey).

hash160(Binary) ->
    crypto:hash(ripemd160, crypto:hash(sha256, Binary)).

fingerprint(PublicKey) ->
    <<Fingerprint:32, _/binary>> = key_identifier(PublicKey),
    Fingerprint.

key_identifier(PublicKey) ->
    hash160(pack_point(PublicKey)).

%% @doc Multiply the curves base point P times
point(P) ->
    {_, {A,_ , _}, Point0, _, _} = curve_params(),
    Point = unpack_point(Point0),
    point_mul(Point, P, crypto:bytes_to_integer(A)).

%% @doc Get the parameters for the secp256k1 curve
curve_params() ->
    crypto:ec_curve(curve()).

%% @doc The secp256k1 curve
curve() ->
    secp256k1.

%% @doc Pack a compressed point
pack_point({even, X}) -> <<16#02, X:256>>;
pack_point({odd, X})  -> <<16#03, X:256>>;
pack_point({X,Y})     -> <<16#04, X:256, Y:256>>.

%% @doc Unpack a point
unpack_point(<<16#02, X:256>>)        -> {even, X};
unpack_point(<<16#03, X:256>>)        -> {odd, X};
unpack_point(<<16#04, X:256, Y:256>>) -> {X, Y}.

%% @doc Point addition 
point_add({X1, Y1}, {X2, Y2}) ->
    Lambda = (Y2 - Y1) div (X2 - X1),
    X3 = Lambda * Lambda - X1 - X2,
    Y3 = Lambda * (X1 - X3) - Y1,
    {X3, Y3}.

%% @doc Point doubling
point_double({X1,Y1}, A) ->
    Lambda = (3 * X1 * X1 + A) div (2 * Y1),
    X2 = Lambda * Lambda - 2 * X1,
    Y2 = Lambda * (X1 - X2) - Y1,
    {X2, Y2}.

%% @doc Point multiplication
%%      Double-add method
point_mul(Point, 1, _) ->
    Point;
point_mul(Point, N, A) ->
    case N rem 2 == 1 of
        true  -> point_add(Point, point_mul(Point, N-1, A));
        false -> point_mul(point_double(Point, A), N div 2, A)
    end.

