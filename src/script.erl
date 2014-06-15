-module(script).

-include("ecoin.hrl").
-include("script.hrl").

-export([create_env/3,
         run/2,
         is_valid/1,
         encode/1,
         decode/1]).

-record(env, {
          primary   = stack:new() :: stack:stack(),
          secondary = stack:new() :: stack:stack(),
          tx                      :: #tx{},
          index                   :: non_neg_integer(),
          codesep = 0             :: non_neg_integer(),
          script                  :: binary()
         }).

create_env(Tx, Index, Script) ->
    #env{tx=Tx, index=Index, script=Script}.

run(Script, Env) ->
   lists:foldl(fun step/2, Env, Script).

is_valid(Env) ->
    unpack_bool(stack:peek(Env#env.primary)).

encode(Script) ->
    iolist_to_binary(lists:map(fun encode_op/1, Script)).

decode(Script) ->
    decode(Script, 0, []).

step(Op, Env) ->
    undefined.

%% @doc Transform the stack with function F
%%      The function F needs to correctly unpack it's arguments and
%%      return a list of new items to put on the stack
pop_f(Stack0, F) ->
    {arity, N} = erlang:fun_info(F, arity),
    {Args, Stack1} = stack:pop_n(Stack0, N),
    stack:push_n(Stack1, lists:map(fun pack/1, apply(F, Args))).

push(Stack, Data) -> stack:push(Stack, pack(Data)).

bool_f(F) -> fun(A) -> [F(unpack_bool(A))] end.
bool2_f(F) -> fun(A, B) -> [F(unpack_bool(A), unpack_bool(B))] end.

bitwise_f(F) ->
    fun(A) ->
            Size = bit_size(A),
            <<Int:Size>> = A,
            [<<(F(Int)):Size>>]
    end.

bitwise_f2(F) ->
    fun(A, B) ->
            Size = bit_size(A),
            {<<AInt:Size>>, <<BInt:Size>>} = {A, B},
            [<<(F(AInt, BInt)):Size>>]
    end.

int_f(F) -> fun(A) -> [F(unpack_varint(A))] end.
int2_f(F) -> fun(A, B) -> [F(unpack_varint(A), unpack_varint(B))] end.
int3_f(F) ->
    fun(A, B, C) ->
            [F(unpack_varint(A), unpack_varint(B), unpack_varint(C))]
    end.

crypto_f(HashAlgo) -> fun(A) -> crypto:hash(HashAlgo, A) end.

%% @doc Run the next operation in the script, returning a new state
stack_step(S, {push, I}) -> push(S, I);
stack_step(S, verify) -> pop_f(S, fun(B) -> [true = unpack_bool(B)] end);
stack_step(S, ifdup) ->
    pop_f(S, fun(B) -> case unpack_bool(B) of
                           true  -> [B, B];
                           false -> [B]
                       end
             end);
stack_step(S, depth) -> push(S, stack:size(S));
stack_step(S, {drop, N}) -> element(2, stack:pop_n(S, N));
stack_step(S, {dup, N}) -> stack:push_n(S, stack:peek_n(S, N));
stack_step(S, nip) -> element(2, stack:pop_ix(S, 2));
stack_step(S, {over, N}) -> stack:push_n(S, stack:peek_part(S, N*1, N));
stack_step(S0, pick) ->
    {N, S1} = stack:pop(S0),
    stack:push(stack:peek_ix(unpack_varint(N), S1), S1);
stack_step(S0, roll) ->
    {N, S1} = stack:pop(S0),
    {I, S2} = stack:pop_ix(S1, unpack_varint(N)),
    stack:push(I, S2);
stack_step(S0, {rot, N}) ->
    {F, S1} = stack:pop_n(S0, N*3),
    {T1, T2} = lists:split(N*2, F),
    stack:push_n(S1, T2++T1);
stack_step(S0, {swap, N}) ->
    {F, S1} = stack:pop_n(S0, N*2),
    {T1, T2} = lists:split(N, F),
    stack:push_n(S1, T2++T1);
stack_step(S, tuck) -> pop_f(S, fun(A,B) -> [A, B, A] end);
stack_step(S, cat) -> pop_f(S, fun(A, B) -> [<<B/binary, A/binary>>] end);
stack_step(S, substr) ->
    pop_f(S, fun(Sz, B, I) ->
                     [binary:part(I, unpack_varint(B), unpack_varint(Sz))]
             end);
stack_step(S, left) ->
    pop_f(S, fun(Ix, I) -> [binary:part(I, 0, unpack_varint(Ix))] end);
stack_step(S, right) ->
    pop_f(S, fun(Ix, I) ->
                     Index = unpack_varint(Ix),
                     [binary:part(I, Index+1, byte_size(I)-Index-1)]
          end);
stack_step(S, size) -> push(S, byte_size(stack:peek(S)));
stack_step(S, invert) -> pop_f(S, bitwise_f(fun(A) -> bnot A end));
stack_step(S, 'band') -> pop_f(S, bitwise_f(fun(A, B) -> A band B end));
stack_step(S, 'bor') -> pop_f(S, bitwise_f(fun(A, B) -> A bor B end));
stack_step(S, 'bxor') -> pop_f(S, bitwise_f(fun(A, B) -> A bxor B end));
stack_step(S, equal) -> pop_f(S, fun(A, B) -> [A==B] end);
stack_step(S, add1) -> pop_f(S, int_f(fun(A) -> A+1 end)); 
stack_step(S, sub1) -> pop_f(S, int_f(fun(A) -> A-1 end));
stack_step(S, mul2) -> pop_f(S, int_f(fun(A) -> A*2 end));
stack_step(S, div2) -> pop_f(S, int_f(fun(A) -> A div 2 end));
stack_step(S, negate) -> pop_f(S,int_f(fun(A) -> A*-1 end));
stack_step(S, abs) -> pop_f(S, int_f(fun abs/1));
stack_step(S, 'not') -> pop_f(S, bool_f(fun(A) -> not A end));
stack_step(S, notequal0) -> pop_f(S, bool_f(fun(A) -> A end));
stack_step(S, add) -> pop_f(S, int2_f(fun(A, B) -> A+B end));
stack_step(S, sub) -> pop_f(S, int2_f(fun(A, B) -> B-A end));
stack_step(S, mul) -> pop_f(S, int2_f(fun(A, B) -> A*B end));
stack_step(S, divide) -> pop_f(S, int2_f(fun(A, B) -> B div A end));
stack_step(S, mod) -> pop_f(S, int2_f(fun(A, B) -> B rem A end));
stack_step(S, lshift) ->
    pop_f(S, int2_f(fun(A, B) -> sign(A)*(abs(A) bsl B) end));
stack_step(S, rshift) ->
    pop_f(S, int2_f(fun(A, B) -> sign(A)*(abs(A) bsr B) end));
stack_step(S, 'and') -> pop_f(S, bool2_f(fun(A, B) -> A andalso B end));
stack_step(S, 'or') -> pop_f(S, bool2_f(fun(A, B) -> A orelse B end));
stack_step(S, num_equal) -> pop_f(S, int2_f(fun(A, B) -> A == B end));
stack_step(S, num_notequal) -> pop_f(S, int2_f(fun(A, B) -> A /= B end));
stack_step(S, lessthan) -> pop_f(S, int2_f(fun(A, B) -> B < A end));
stack_step(S, greaterthan) -> pop_f(S, int2_f(fun(A, B) -> B > A end));
stack_step(S, lessthanorequal) -> pop_f(S, int2_f(fun(A, B) -> B =< A end));
stack_step(S, greaterthanorequal) -> pop_f(S, int2_f(fun(A, B) -> B >= A end));
stack_step(S, min) -> pop_f(S, int2_f(fun(A, B) -> min(A, B) end));
stack_step(S, max) -> pop_f(S, int2_f(fun(A, B) -> max(A, B) end));
stack_step(S, within) ->
    pop_f(S, int3_f(fun(Max, Min, X) -> X >= Min andalso X < Max end));
stack_step(S, ripemd160) -> pop_f(S, crypto_f(ripemd160));
stack_step(S, sha1) -> pop_f(S, crypto_f(sha1));
stack_step(S, sha256) -> pop_f(S, crypto_f(sha256));
stack_step(S, hash160) -> pop_f(S, fun ecoin_crypto:hash160/1);
stack_step(S, hash256) -> pop_f(S, fun ecoin_crypto:hash256/1).

sign(Int) when Int < 0 -> -1;
sign(_)                ->  1.

pack(Bool) when is_boolean(Bool) -> pack_bool(Bool);
pack(Integer) when is_integer(Integer) -> pack_varint(Integer);
pack(Binary) when is_binary(Binary) -> Binary.

%% @doc Pack a bool as a binary
pack_bool(true)  -> <<1>>;
pack_bool(false) -> <<>>.

%% @doc Unpack a binary as a bool
unpack_bool(Data) ->
    case unpack_varint(Data) of
        0 -> false;
        _ -> true
    end.

%% @doc Pack a variable size little endian integer
pack_varint(0) ->
    <<>>;
pack_varint(Int) when Int < 0 ->
    pack_varint(abs(Int), negative);
pack_varint(Int) ->
    pack_varint(Int, positive).

pack_varint(UInt, Sign) ->
    Bin0 = binary:encode_unsigned(UInt, little),
    %% Pad with an extra byte if it overflows
    case bit7(binary:last(Bin0)) of
        one ->
            case Sign of
                positive -> <<Bin0/binary, 0>>;
                negative -> <<Bin0/binary, 16#80>>
            end;
        zero ->
            case Sign of
                positive -> Bin0;
                negative ->
                    AllButLast = byte_size(Bin0)-1,
                    <<AllButLastByte:AllButLast/binary, LastByte>> = Bin0,
                    <<AllButLastByte/binary, (LastByte+16#80)>>
            end
    end.

%% @doc Unpack a variable size little endian integer
%%      Most significant bit in last byte encodes sign,
%%      if the number overflows, add an extra byte in front of it
unpack_varint(<<>>) ->
    0;
unpack_varint(Bin0) when byte_size(Bin0) =< 4->
    AllButLast = byte_size(Bin0)-1,
    <<AllButLastByte:AllButLast/binary, LastByte0>> = Bin0,
    {Sign, LastByte1}= case bit7(LastByte0) of
                           one  -> {-1, LastByte0 - 16#80};
                           zero -> {1,  LastByte0}
                       end,
    Sign * binary:decode_unsigned(<<AllButLastByte/binary, LastByte1>>, little).

bit7(Byte) when (Byte band 16#80) == 0 -> zero;
bit7(_)                                -> one.

pack_op(Op) ->
    case Op of
        op_true                 -> ?OP_TRUE;
        op_false                -> ?OP_FALSE;
        op_1negate              -> ?OP_1NEGATE;
        {op_n, N}               -> N + ?OP_1 - 1;
        {op_pushdata, Data}     -> pack_op_pushdata(Data);
        op_nop                  -> ?OP_NOP;
        {op_if, True, False}    -> [?OP_IF    | pack_op_if(True, False)];
        {op_notif, False, True} -> [?OP_NOTIF | pack_op_if(False, True)];
        op_verify               -> ?OP_VERIFY;
        op_return               -> ?OP_RETURN;
        op_toaltstack           -> ?OP_TOALTSTACK;
        op_fromaltstack         -> ?OP_FROMALTSTACK;
        op_ifdup                -> ?OP_IFDUP;
        op_depth                -> ?OP_DEPTH;
        {op_drop, 1}            -> ?OP_DROP;
        {op_drop, 2}            -> ?OP_2DROP;
        {op_dup, 1}             -> ?OP_DUP;
        {op_dup, 2}             -> ?OP_2DUP;
        {op_dup, 3}             -> ?OP_3DUP;
        op_nip                  -> ?OP_NIP;
        {op_over, 1}            -> ?OP_OVER;
        {op_over, 2}            -> ?OP_2OVER;
        op_pick                 -> ?OP_PICK;
        op_roll                 -> ?OP_ROLL;
        {op_rot, 1}             -> ?OP_ROT;
        {op_rot, 2}             -> ?OP_2ROT;
        {op_swap, 1}            -> ?OP_SWAP;
        {op_swap, 2}            -> ?OP_2SWAP;
        op_tuck                 -> ?OP_TUCK;
        op_cat                  -> ?OP_CAT;
        op_substr               -> ?OP_SUBSTR;
        op_left                 -> ?OP_LEFT;
        op_right                -> ?OP_RIGHT;
        op_size                 -> ?OP_SIZE;
        op_invert               -> ?OP_INVERT;
        op_and                  -> ?OP_AND;
        op_or                   -> ?OP_OR;
        op_xor                  -> ?OP_XOR;
        op_equal                -> ?OP_EQUAL;
        op_equalverify          -> ?OP_EQUALVERIFY;
        op_1add                 -> ?OP_1ADD;
        op_1sub                 -> ?OP_1SUB;
        op_2mul                 -> ?OP_2MUL;
        op_2div                 -> ?OP_2DIV;
        op_negate               -> ?OP_NEGATE;
        op_abs                  -> ?OP_ABS;
        op_not                  -> ?OP_NOT;
        op_0notequal            -> ?OP_0NOTEQUAL;
        op_add                  -> ?OP_ADD;
        op_sub                  -> ?OP_SUB;
        op_mul                  -> ?OP_MUL;
        op_div                  -> ?OP_DIV;
        op_mod                  -> ?OP_MOD;
        op_lshift               -> ?OP_LSHIFT;
        op_rshift               -> ?OP_RSHIFT;
        op_booland              -> ?OP_BOOLAND;
        op_boolor               -> ?OP_BOOLOR;
        op_numequal             -> ?OP_NUMEQUAL;
        op_numequalverify       -> ?OP_NUMEQUALVERIFY;
        op_numnotequal          -> ?OP_NUMNOTEQUAL;
        op_lessthan             -> ?OP_LESSTHAN;
        op_greaterthan          -> ?OP_GREATERTHAN;
        op_lessthanorequal      -> ?OP_LESSTHANOREQUAL;
        op_greaterthanorequal   -> ?OP_GREATERTHANOREQUAL;
        op_min                  -> ?OP_MIN;
        op_max                  -> ?OP_MAX;
        op_within               -> ?OP_WITHIN;
        op_ripemd160            -> ?OP_RIPEMD160;
        op_sha1                 -> ?OP_SHA1;
        op_sha256               -> ?OP_SHA256;
        op_hash160              -> ?OP_HASH160;
        op_hash256              -> ?OP_HASH256;
        op_codeseparator        -> ?OP_CODESEPARATOR;
        op_checksig             -> ?OP_CHECKSIG;
        op_checksigverify       -> ?OP_CHECKSIGVERIFY;
        op_pubkeyhash           -> ?OP_PUBKEYHASH;
        op_pubkey               -> ?OP_PUBKEY;
        op_invalidopcode        -> ?OP_INVALIDOPCODE;
        op_reserved             -> ?OP_RESERVED;
        op_ver                  -> ?OP_VER;
        op_verif                -> ?OP_VERIF;
        op_vernotif             -> ?OP_VERNOTIF;
        op_reserved1            -> ?OP_RESERVED1;
        op_reserved2            -> ?OP_RESERVED2;
        op_nop1                 -> ?OP_NOP1;
        op_nop2                 -> ?OP_NOP2;
        op_nop3                 -> ?OP_NOP3;
        op_nop4                 -> ?OP_NOP4;
        op_nop5                 -> ?OP_NOP5;
        op_nop6                 -> ?OP_NOP6;
        op_nop7                 -> ?OP_NOP7;
        op_nop8                 -> ?OP_NOP8;
        op_nop9                 -> ?OP_NOP9;
        op_nop10                -> ?OP_NOP10
    end.

%% @doc Pack a the correct pushdata operation depening on data size
pack_op_pushdata(Data) -> 
    Size = byte_size(Data),
    case Size of
        _ when ?OP_PUSHDATA(Size) ->
            [Size, Data];
        _ when Size =< 16#ff ->
            [?OP_PUSHDATA1, Size, Data];
        _ when Size =< 16#ffff ->
            [?OP_PUSHDATA2, <<Size:16>>, Data];
        _ when Size =< 16#ffffffff ->
            [?OP_PUSHDATA4, <<Size:32>>, Data]
    end.

%% @doc Pack an notif/if operation
pack_op_if(BranchA, BranchB) ->
    [lists:map(fun pack_op/1, BranchA),
     ?OP_ELSE,
     lists:map(fun pack_op/1, BranchB),
     ?OP_ENDIF].

%% @doc Decode a script
decode(<<>>, _, Acc) ->
    lists:reverse(Acc);
decode(<<?OP_CODESEPARATOR, Script/binary>>, Pos, Acc) ->
    decode(Script, Pos+1, [{codeseparator, Pos}|Acc]);
decode(<<?OP_IF, Script0/binary>>, Pos0, Acc) ->
    {True, False, Pos1, Script1} = decode_if(Script0, Pos0+1),
    decode(Script1, Pos1, [{'if', True, False}|Acc]);
decode(<<?OP_NOTIF, Script0/binary>>, Pos0, Acc) ->
    {False, True, Pos1, Script1} = decode_if(Script0, Pos0+1),
    decode(Script1, Pos1, [{notif, False, True}|Acc]);
decode(<<?OP_ELSE, Script/binary>>, Pos, Acc) ->
    {else, lists:reverse(Acc), Pos+1, Script};
decode(<<?OP_ENDIF, Script/binary>>, Pos, Acc) ->
    {endif, lists:reverse(Acc), Pos+1, Script};
decode(Script0, Pos, Acc) ->
    {Op, Len, Script1} = decode_op(Script0),
    decode(Script1, Pos+Len, [Op|Acc]).

%% @doc Decode a (n)if operation
decode_if(Script0, Pos0) ->
    case decode(Script0, Pos0, []) of
        {else, BranchA, Pos1, Script1} ->
             {endif, BranchB, Pos2, Script2} = decode(Script1, Pos1, []),
             {BranchA, BranchB, Pos2, Script2};
        {endif, BranchA, Pos1, Script1} ->
            {BranchA, [], Pos1, Script1}
    end.

%% @doc Decode a script operation
decode_op(<<Size, Script0/binary>>) when ?OP_PUSHDATA(Size) ->
    <<Data:Size/binary, Script1/binary>> = Script0,
    {{op_pushdata, Data}, 1+Size, Script1};
decode_op(<<Op, Script0/binary>>) when Op == ?OP_PUSHDATA1;
                                       Op == ?OP_PUSHDATA2;
                                       Op == ?OP_PUSHDATA4 ->
    SizeBytes = case Op of
                    ?OP_PUSHDATA1 -> 1;
                    ?OP_PUSHDATA2 -> 2;
                    ?OP_PUSHDATA4 -> 4
                end, 
    SizeBits = SizeBytes * 8,
    <<Size:SizeBits,    Script1/binary>> = Script0,
    <<Data:Size/binary, Script2/binary>> = Script1,
    {{op_pushdata, Data}, 1+SizeBytes+Size, Script2};
decode_op(<<OpCode, Script/binary>>) ->
    Op = case OpCode of
             ?OP_TRUE                -> {push, true};
             ?OP_FALSE               -> {push, false};
             ?OP_1NEGATE             -> {push, -1};
             _ when ?OP_N(OpCode)    -> {push, OpCode-?OP_1+1};
             ?OP_NOP                 -> noop;
             ?OP_VERIFY              -> verify;
             ?OP_RETURN              -> return;
             ?OP_TOALTSTACK          -> tosecondary;
             ?OP_FROMALTSTACK        -> fromsecondary;
             ?OP_IFDUP               -> ifduplicate;
             ?OP_DEPTH               -> depth;
             ?OP_DROP                -> {drop, 1};
             ?OP_2DROP               -> {drop, 2};
             ?OP_DUP                 -> {duplicate, 1};
             ?OP_2DUP                -> {duplicate, 2};
             ?OP_3DUP                -> {duplicate, 3};
             ?OP_NIP                 -> nip;
             ?OP_OVER                -> {over, 1};
             ?OP_2OVER               -> {over, 2};
             ?OP_PICK                -> pick;
             ?OP_ROLL                -> roll;
             ?OP_ROT                 -> {rotate, 1};
             ?OP_2ROT                -> {rotate, 2};
             ?OP_SWAP                -> {swap, 1};
             ?OP_2SWAP               -> {swap, 2};
             ?OP_TUCK                -> tuck;
             ?OP_CAT                 -> concatenate;
             ?OP_SUBSTR              -> substring;
             ?OP_LEFT                -> left;
             ?OP_RIGHT               -> right;
             ?OP_SIZE                -> size;
             ?OP_INVERT              -> invert;
             ?OP_AND                 -> bitwiseand;
             ?OP_OR                  -> bitwiseor;
             ?OP_XOR                 -> bitwisexor;
             ?OP_EQUAL               -> equal;
             ?OP_EQUALVERIFY         -> equal_verify;
             ?OP_1ADD                -> add1;
             ?OP_1SUB                -> sub1;
             ?OP_2MUL                -> mul2;
             ?OP_2DIV                -> div2;
             ?OP_NEGATE              -> negate;
             ?OP_ABS                 -> abs;
             ?OP_NOT                 -> 'not';
             ?OP_0NOTEQUAL           -> notequal0;
             ?OP_ADD                 -> add;
             ?OP_SUB                 -> subtract;
             ?OP_MUL                 -> multiply;
             ?OP_DIV                 -> divide;
             ?OP_MOD                 -> modulus;
             ?OP_LSHIFT              -> leftshift;
             ?OP_RSHIFT              -> rightshift;
             ?OP_BOOLAND             -> booland;
             ?OP_BOOLOR              -> boolor;
             ?OP_NUMEQUAL            -> numequal;
             ?OP_NUMNOTEQUAL         -> notnumequal;
             ?OP_NUMEQUALVERIFY      -> numequalverify;
             ?OP_LESSTHAN            -> lessthan;
             ?OP_LESSTHANOREQUAL     -> lessthanorequal;
             ?OP_GREATERTHAN         -> greaterthan;
             ?OP_GREATERTHANOREQUAL  -> greaterthanorequal;
             ?OP_MIN                 -> min;
             ?OP_MAX                 -> max;
             ?OP_WITHIN              -> within;
             ?OP_RIPEMD160           -> ripemd160;
             ?OP_SHA1                -> sha1;
             ?OP_SHA256              -> sha256;
             ?OP_HASH160             -> hash160;
             ?OP_HASH256             -> hash256;
             ?OP_CHECKSIG            -> checksig;
             ?OP_CHECKSIGVERIFY      -> checksigverify;
             ?OP_CHECKMULTISIG       -> checkmultisig;
             ?OP_CHECKMULTISIGVERIFY -> checkmultisigverify
         end,
    {Op, 1, Script}.
