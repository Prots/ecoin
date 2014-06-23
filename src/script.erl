-module(script).

-include("ecoin.hrl").
-include("script.hrl").

-export([create_env/3,
         run/2,
         is_valid/1,
         %encode/1,
         decode/1]).

-record(env, {
          primary   = stack:new() :: stack:stack(),
          secondary = stack:new() :: stack:stack(),
          transaction             :: tx:transaction(),
          index                   :: non_neg_integer(),
          codeseparator = 0       :: non_neg_integer(),
          script                  :: script()
         }).

%% @doc Create a new environment to run a script in
create_env(Tx, Index, Script) ->
    #env{transaction=Tx,
         index=Index,
         script=Script}.

%% @doc Run a script in an environment
run(Script, Env) ->
   lists:foldl(fun step/2, Env, Script).

%% @doc Check if a script ran successfully
is_valid(Env) ->
    unpack_bool(stack:peek(Env#env.primary)).

%encode(Script) ->
    %iolist_to_binary(lists:map(fun encode_op/1, Script)).

%% @doc Decode a script
decode(Script) -> 
    decode(Script, 0, []).

%% @doc Pop the head of the primary stack as a boolean
pop_head_bool(#env{primary=Primary0} = Env) ->
    {Head, Primary1} = stack:pop(Primary0),
    {unpack_bool(Head), Env#env{primary=Primary1}}.

step({'if', True, False}, Env0) ->
    step_if(True, False, Env0);
step({notif, False, True}, Env0) ->
    step_if(True, False, Env0);
step(nop, Env) ->
    Env;
step({codeseparator, Index}, Env) ->
    Env#env{codeseparator=Index};
step(return, Env) ->
    throw({invalid, return, Env});
step(tosecondary, #env{primary=Primary0, secondary=Secondary} = Env) ->
    {Head, Primary1} = stack:pop(Primary0),
    Env#env{primary=Primary1, secondary=stack:push(Secondary, Head)};
step(fromsecondary, #env{primary=Primary, secondary=Secondary0} = Env) ->
    {Head, Secondary1} = stack:pop(Secondary0),
    Env#env{primary=stack:push(Primary, Head), secondary=Secondary1};
step(checksig, Env) ->
    checksig(Env);
step(checksigverify, Env) ->
    step(verify, step(checksig, Env));
step(checkmultisig, _Env) ->
    undefined;
step(checkmultisigverify, Env) ->
    step(verify, step(checkmultisig, Env));
step(StackOp, #env{primary=Stack} = Env) ->
    Env#env{primary=stack_step(Stack, StackOp)}.

step_if(BranchA, BranchB, Env0) ->
    {Bool, Env1} = pop_head_bool(Env0),
    Branch = case Bool of
                 true  -> BranchA;
                 false -> BranchB
             end,
    run(Branch, Env1).

checksig(#env{ primary = Primary0,
               script = Script,
               codeseparator = CS,
               transaction = Tx,
               index = Ix } = Env) ->
    {[PubKey, Sig0], Primary1} = stack:pop_n(Primary0, 2),
    AllButLast = byte_size(Sig0) - 1,
    <<Sig1:AllButLast, HashTypeByte>> = Sig0,
    SubScript = subscript(Script, Sig1, CS),
    TxCopy = txcopy(Tx, Ix, SubScript),
    Hash = hash(TxCopy, Ix, HashTypeByte),
    Res = crypto:verify(ecdsa, sha256, Hash, Sig1, PubKey),
    Env#env{ primary = push(Primary1, Res) }.

hash(#tx{ in = Inputs, out = Outputs0 } = Tx0, Index, HashTypeByte) ->
    [HashType | AnyOneCanPay] = decode_hashtype(HashTypeByte),
    Tx1 = case HashType of
              all    -> Tx0;
              none   -> sighash_none(Tx0, Index);
              single -> sighash_single(Tx0, Index)
          end,
    Tx2 = case AnyOneCanPay of
              [] -> Tx1;
              [anyonecanpay] -> sighash_anyonecanpay(Tx1, Index)
          end,
    ecoin_crypto:hash256(<<(tx:encode(Tx2))/binary, HashTypeByte:32/little>>).    


sighash_none(#tx{ in = Inputs } = Tx, Index) ->
    SigHashNone = fun (Ix, Input) when Index == Ix -> Input;
                      (_, Input) -> Input#tx_in{ sequence = 0 }
                  end,
    Tx#tx{ in  = array:map(SigHashNone, Inputs), out = array:new(0) }.

sighash_single(#tx{out = Outputs0} = Tx, Index) ->
    {NewOutputList, _} = lists:split(Index, array:to_list(Outputs0)),
    Outputs1 = array:from_list(NewOutputList),
    SigHashSingle = fun (Ix, Output) when Index == Ix -> Output;
                        (_, Output) -> Output#tx_out{ value = -1, script = <<>> }
                    end,
    Tx0#tx{ out = array:map(Blank, Outputs1) }.

sighash_anyonecanpay(#tx{ in = Inputs } = Tx, Index) ->
    Tx#tx{in=array:set(0, array:get(Index, Inputs), array:new(1))}.

subscript(Script, Sig, CS0) ->
    End = byte_size(Script),
    SubScript = binary:part(Script, CS0, End),
    iolist_to_binary(clean(SubScript, Sig)).

txcopy(#tx{in=Inputs} = Tx, Ix, SubScript) ->
    SetSubScript = fun(Index, Input) when Index == Ix -> Input#tx_in{script=SubScript};
                      (_, Input) -> Input#tx_in{script = <<>>}
                   end,
    Tx#tx{in=array:map(SetSubScript, Inputs)}.

clean(<<>>, _) ->
    [];
clean(<<Size, Sig:Size/binary, SubScript/binary>>, Sig) when ?OP_PUSHDATA(Size) ->
    clean(SubScript, Sig);
clean(<<Size, Data:Size/binary, SubScript/binary>>, Sig) when ?OP_PUSHDATA(Size) ->
    [Size, Data | clean(SubScript, Sig)];
clean(<<?OP_PUSHDATA1, Size, Sig:Size/binary, SubScript/binary>>, Sig) ->
    clean(SubScript, Sig);
clean(<<?OP_PUSHDATA1, Size, Data:Size/binary, SubScript/binary>>, Sig) ->
    [?OP_PUSHDATA1, Size, Data | clean(SubScript, Sig)];
clean(<<?OP_PUSHDATA2, Size:16/little, Sig:Size/binary, SubScript/binary>>, Sig) ->
    clean(SubScript, Sig);
clean(<<?OP_PUSHDATA2, Size:16/little, Data:Size/binary, SubScript/binary>>, Sig) ->
    [?OP_PUSHDATA2, <<Size:16/little>>, Data | clean(SubScript, Sig)];
clean(<<?OP_PUSHDATA4, Size:32/little, Sig:Size/binary, SubScript/binary>>, Sig) ->
    clean(SubScript, Sig);
clean(<<?OP_PUSHDATA4, Size:32/little, Data:Size/binary, SubScript/binary>>, Sig) ->
    [?OP_PUSHDATA4, <<Size:32/little>>, Data | clean(SubScript, Sig)];
clean(<<?OP_CODESEPARATOR, SubScript/binary>>, Sig) ->
    clean(SubScript, Sig);
clean(<<Op, SubScript>>, Sig) ->
    [Op | clean(SubScript, Sig)].

decode_hashtype(HashType) ->
    TypeInt = HashType band 31,
    Type = case TypeInt of
               0 -> all;
               ?SIGHASH_ALL -> all;
               ?SIGHASH_NONE -> none;
               ?SIGHASH_SINGLE -> single
           end,
    case HashType band ?SIGHASH_ANYONECANPAY of
        0 -> [Type];
        ?SIGHASH_ANYONECANPAY -> [Type, anyonecanpay]
    end.

%encode_hashtype(all) ->
    %?SIGHASH_ALL;
%encode_hashtype(none) ->
    %?SIGHASH_NONE;
%encode_hashtype(single) ->
    %?SIGHASH_SINGLE;
%encode_hashtype(anyonecanpay) ->
    %?SIGHASH_ANYONECANPAY.

%% @doc Transform the stack with function F
%%      The function F needs to correctly unpack it's arguments and
%%      return a list of new items to put on the stack
pop_f(Stack0, F) ->
    {arity, N} = erlang:fun_info(F, arity),
    {Args, Stack1} = stack:pop_n(Stack0, N),
    case apply(F, Args) of
        Res when is_list(Res) -> 
            stack:push_n(Stack1, lists:map(fun pack/1, Res));
        Res ->
            push(Stack1, Res)
    end.

push(Stack, Data) ->
    stack:push(Stack, pack(Data)).

bitwise_f(F) ->
    {arity, N} = erlang:fun_info(F, arity),
    case N of
        1 -> bitwise1_f(F);
        2 -> bitwise2_f(F)
    end.

bitwise1_f(F) ->
    fun(A) ->
            Size = bit_size(A),
            <<Int:Size>> = A,
            <<(F(Int)):Size>>
    end.

bitwise2_f(F) ->
    fun(A, B) ->
            Size = bit_size(A),
            {<<AInt:Size>>, <<BInt:Size>>} = {A, B},
            <<(F(BInt, AInt)):Size>>
    end.

type_fun(Type, F) ->
    {arity, N} = erlang:fun_info(F, arity),
    type_fun(Type, N, F).

type_fun(Type, 1, F) ->
    fun(A) -> F(unpack(Type, A)) end;
type_fun(Type, 2, F) ->
    fun(A, B) -> F(unpack(Type, B), unpack(Type, A)) end;
type_fun(Type, 3, F) ->
    fun(A, B, C) ->
            F(unpack(Type, C), unpack(Type, B), unpack(Type, A))
    end.

bool_f(F) ->
    type_fun(bool, F).

int_f(F) ->
    type_fun(varint, F).

crypto_f(HashAlgo) ->
    fun(A) -> crypto:hash(HashAlgo, A) end.

%% @doc Run the next operation in the script, returning a new state
stack_step(S, {push, Item})->
    push(S, Item);
stack_step(S, verify) ->
    Verify = fun(true)  -> true;
                (false) -> throw({invalid, verify})
             end,
    pop_f(S, bool_f(Verify));
stack_step(S, ifduplicate) ->
    IfDuplicate = fun(B) ->
                          case unpack(bool, B) of
                              true  -> [B, B];
                              false -> B
                          end
                  end,
    pop_f(S, IfDuplicate);
stack_step(S, depth) ->
    push(S, stack:size(S));
stack_step(S, {drop, N}) ->
    element(2, stack:pop_n(S, N));
stack_step(S, {duplicate, N}) ->
    stack:push_n(S, stack:peek_n(S, N));
stack_step(S, nip) ->
    element(2, stack:pop_ix(S, 2));
stack_step(S, {over, N}) ->
    stack:push_n(S, stack:peek_part(S, N*1, N));
stack_step(S0, pick) ->
    {N, S1} = stack:pop(S0),
    stack:push(stack:peek_ix(unpack(varint, N), S1), S1);
stack_step(S0, roll) ->
    {N, S1} = stack:pop(S0),
    {I, S2} = stack:pop_ix(S1, unpack(varint, N)),
    stack:push(I, S2);
stack_step(S0, {rotate, N}) ->
    {F, S1} = stack:pop_n(S0, N*3),
    {T1, T2} = lists:split(N*2, F),
    stack:push_n(S1, T2++T1);
stack_step(S0, {swap, N}) ->
    {F, S1} = stack:pop_n(S0, N*2),
    {T1, T2} = lists:split(N, F),
    stack:push_n(S1, T2++T1);
stack_step(S, tuck) ->
    pop_f(S, fun(A,B) -> [A, B, A] end);
stack_step(S, concatenate) ->
    Concatenate = fun(A, B) -> <<B/binary, A/binary>> end,
    pop_f(S, Concatenate);
stack_step(S, substring) ->
    SubString = fun(Size, Begin, Binary) ->
                        binary:part(Binary,
                                    unpack(varint, Begin),
                                    unpack(varint, Size))
                end,
    pop_f(S, SubString);
stack_step(S, leftof) ->
    LeftOf = fun(Index, Binary) ->
                     binary:part(Binary, 0, unpack(varint, Index))
             end,
    pop_f(S, LeftOf);
stack_step(S, rightof) ->
    RightOf = fun(Index, Binary) ->
                     Ix = unpack_varint(Index),
                     binary:part(Binary, Ix+1, byte_size(Binary)-Ix-1)
              end,
    pop_f(S, RightOf);
stack_step(S, size) ->
    push(S, byte_size(stack:peek(S)));
stack_step(S, invert) ->
    pop_f(S, bitwise_f(fun erlang:'bnot'/1));
stack_step(S, bitwiseand) ->
    pop_f(S, bitwise_f(fun erlang:'band'/2));
stack_step(S, bitwiseor) ->
    pop_f(S, bitwise_f(fun erlang:'bor'/2));
stack_step(S, bitwisexor) ->
    pop_f(S, bitwise_f(fun erlang:'bxor'/2));
stack_step(S, equal) ->
    pop_f(S, fun erlang:'=:='/2);
stack_step(S, increment) ->
    Increment = fun(A) -> A+1 end,
    pop_f(S, int_f(Increment)); 
stack_step(S, decrement) ->
    Decrement = fun(A) -> A-1 end,
    pop_f(S, int_f(Decrement));
stack_step(S, double) ->
    Double = fun(A) -> A*2 end,
    pop_f(S, int_f(Double));
stack_step(S, halve) ->
    Halve = fun(A) -> A div 2 end,
    pop_f(S, int_f(Halve));
stack_step(S, negate) ->
    Negate = fun(A) -> A*-1 end,
    pop_f(S,int_f(Negate));
stack_step(S, absolutevalue) ->
    pop_f(S, int_f(fun abs/1));
stack_step(S, 'not') ->
    pop_f(S, bool_f(fun erlang:'not'/1));
stack_step(S, notequal0) ->
    NotEqual0 = fun(A) -> A end,
    pop_f(S, bool_f(NotEqual0));
stack_step(S, add) ->
    pop_f(S, int_f(fun erlang:'+'/2));
stack_step(S, subtract) ->
    pop_f(S, int_f(fun(A, B) -> B-A end));
stack_step(S, multiply) -> pop_f(S, int_f(fun(A, B) -> A*B end));
stack_step(S, divide) -> pop_f(S, int_f(fun(A, B) -> B div A end));
stack_step(S, modulus) -> pop_f(S, int_f(fun(A, B) -> B rem A end));
stack_step(S, leftshift) ->
    pop_f(S, int_f(fun(A, B) -> sign(A)*(abs(A) bsl B) end));
stack_step(S, rightshift) ->
    pop_f(S, int_f(fun(A, B) -> sign(A)*(abs(A) bsr B) end));
stack_step(S, booland) -> pop_f(S, bool_f(fun(A, B) -> A andalso B end));
stack_step(S, boolor) -> pop_f(S, bool_f(fun(A, B) -> A orelse B end));
stack_step(S, numequal) -> pop_f(S, int_f(fun(A, B) -> A == B end));
stack_step(S, notnumequal) -> pop_f(S, int_f(fun(A, B) -> A /= B end));
stack_step(S, numequalverify) -> stack_step(stack_step(S, numequal), verify);
stack_step(S, lessthan) -> pop_f(S, int_f(fun(A, B) -> B < A end));
stack_step(S, lessthanorequal) -> pop_f(S, int_f(fun(A, B) -> B =< A end));
stack_step(S, greaterthan) -> pop_f(S, int_f(fun(A, B) -> B > A end));
stack_step(S, greaterthanorequal) -> pop_f(S, int_f(fun(A, B) -> B >= A end));
stack_step(S, min) -> pop_f(S, int_f(fun(A, B) -> min(A, B) end));
stack_step(S, max) -> pop_f(S, int_f(fun(A, B) -> max(A, B) end));
stack_step(S, within) ->
    pop_f(S, int_f(fun(Max, Min, X) -> X >= Min andalso X < Max end));
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

unpack(bool, Bool) -> unpack_bool(Bool);
unpack(varint, VarInt) -> unpack_varint(VarInt).

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

%pack_op(Op) ->
    %case Op of
        %op_true                 -> ?OP_TRUE;
        %op_false                -> ?OP_FALSE;
        %op_1negate              -> ?OP_1NEGATE;
        %{op_n, N}               -> N + ?OP_1 - 1;
        %{op_pushdata, Data}     -> pack_op_pushdata(Data);
        %op_nop                  -> ?OP_NOP;
        %{op_if, True, False}    -> [?OP_IF    | pack_op_if(True, False)];
        %{op_notif, False, True} -> [?OP_NOTIF | pack_op_if(False, True)];
        %op_verify               -> ?OP_VERIFY;
        %op_return               -> ?OP_RETURN;
        %op_toaltstack           -> ?OP_TOALTSTACK;
        %op_fromaltstack         -> ?OP_FROMALTSTACK;
        %op_ifdup                -> ?OP_IFDUP;
        %op_depth                -> ?OP_DEPTH;
        %{op_drop, 1}            -> ?OP_DROP;
        %{op_drop, 2}            -> ?OP_2DROP;
        %{op_dup, 1}             -> ?OP_DUP;
        %{op_dup, 2}             -> ?OP_2DUP;
        %{op_dup, 3}             -> ?OP_3DUP;
        %op_nip                  -> ?OP_NIP;
        %{op_over, 1}            -> ?OP_OVER;
        %{op_over, 2}            -> ?OP_2OVER;
        %op_pick                 -> ?OP_PICK;
        %op_roll                 -> ?OP_ROLL;
        %{op_rot, 1}             -> ?OP_ROT;
        %{op_rot, 2}             -> ?OP_2ROT;
        %{op_swap, 1}            -> ?OP_SWAP;
        %{op_swap, 2}            -> ?OP_2SWAP;
        %op_tuck                 -> ?OP_TUCK;
        %op_cat                  -> ?OP_CAT;
        %op_substr               -> ?OP_SUBSTR;
        %op_left                 -> ?OP_LEFT;
        %op_right                -> ?OP_RIGHT;
        %op_size                 -> ?OP_SIZE;
        %op_invert               -> ?OP_INVERT;
        %op_and                  -> ?OP_AND;
        %op_or                   -> ?OP_OR;
        %op_xor                  -> ?OP_XOR;
        %op_equal                -> ?OP_EQUAL;
        %op_equalverify          -> ?OP_EQUALVERIFY;
        %op_1add                 -> ?OP_1ADD;
        %op_1sub                 -> ?OP_1SUB;
        %op_2mul                 -> ?OP_2MUL;
        %op_2div                 -> ?OP_2DIV;
        %op_negate               -> ?OP_NEGATE;
        %op_abs                  -> ?OP_ABS;
        %op_not                  -> ?OP_NOT;
        %op_0notequal            -> ?OP_0NOTEQUAL;
        %op_add                  -> ?OP_ADD;
        %op_sub                  -> ?OP_SUB;
        %op_mul                  -> ?OP_MUL;
        %op_div                  -> ?OP_DIV;
        %op_mod                  -> ?OP_MOD;
        %op_lshift               -> ?OP_LSHIFT;
        %op_rshift               -> ?OP_RSHIFT;
        %op_booland              -> ?OP_BOOLAND;
        %op_boolor               -> ?OP_BOOLOR;
        %op_numequal             -> ?OP_NUMEQUAL;
        %op_numequalverify       -> ?OP_NUMEQUALVERIFY;
        %op_numnotequal          -> ?OP_NUMNOTEQUAL;
        %op_lessthan             -> ?OP_LESSTHAN;
        %op_greaterthan          -> ?OP_GREATERTHAN;
        %op_lessthanorequal      -> ?OP_LESSTHANOREQUAL;
        %op_greaterthanorequal   -> ?OP_GREATERTHANOREQUAL;
        %op_min                  -> ?OP_MIN;
        %op_max                  -> ?OP_MAX;
        %op_within               -> ?OP_WITHIN;
        %op_ripemd160            -> ?OP_RIPEMD160;
        %op_sha1                 -> ?OP_SHA1;
        %op_sha256               -> ?OP_SHA256;
        %op_hash160              -> ?OP_HASH160;
        %op_hash256              -> ?OP_HASH256;
        %op_codeseparator        -> ?OP_CODESEPARATOR;
        %op_checksig             -> ?OP_CHECKSIG;
        %op_checksigverify       -> ?OP_CHECKSIGVERIFY;
        %op_pubkeyhash           -> ?OP_PUBKEYHASH;
        %op_pubkey               -> ?OP_PUBKEY;
        %op_invalidopcode        -> ?OP_INVALIDOPCODE;
        %op_reserved             -> ?OP_RESERVED;
        %op_ver                  -> ?OP_VER;
        %op_verif                -> ?OP_VERIF;
        %op_vernotif             -> ?OP_VERNOTIF;
        %op_reserved1            -> ?OP_RESERVED1;
        %op_reserved2            -> ?OP_RESERVED2;
        %op_nop1                 -> ?OP_NOP1;
        %op_nop2                 -> ?OP_NOP2;
        %op_nop3                 -> ?OP_NOP3;
        %op_nop4                 -> ?OP_NOP4;
        %op_nop5                 -> ?OP_NOP5;
        %op_nop6                 -> ?OP_NOP6;
        %op_nop7                 -> ?OP_NOP7;
        %op_nop8                 -> ?OP_NOP8;
        %op_nop9                 -> ?OP_NOP9;
        %op_nop10                -> ?OP_NOP10
    %end.

%%% @doc Pack a the correct pushdata operation depening on data size
%pack_op_pushdata(Data) -> 
    %Size = byte_size(Data),
    %case Size of
        %_ when ?OP_PUSHDATA(Size) ->
            %[Size, Data];
        %_ when Size =< 16#ff ->
            %[?OP_PUSHDATA1, Size, Data];
        %_ when Size =< 16#ffff ->
            %[?OP_PUSHDATA2, <<Size:16>>, Data];
        %_ when Size =< 16#ffffffff ->
            %[?OP_PUSHDATA4, <<Size:32>>, Data]
    %end.

%%% @doc Pack an notif/if operation
%pack_op_if(BranchA, BranchB) ->
    %[lists:map(fun pack_op/1, BranchA),
     %?OP_ELSE,
     %lists:map(fun pack_op/1, BranchB),
     %?OP_ENDIF].

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
            case decode(Script1, Pos1, []) of
                {endif, BranchB, Pos2, Script2} ->
                    {BranchA, BranchB, Pos2, Script2};
                _ ->
                    error({decode_error, missing_end_if})
            end;
        {endif, BranchA, Pos1, Script1} ->
            {BranchA, [], Pos1, Script1};
        _ ->
            error({decode_error, missing_end_if})
    end.

%% @doc Decode a script operation
decode_op(<<Size, Script0/binary>>) when ?OP_PUSHDATA(Size) ->
    <<Data:Size/binary, Script1/binary>> = Script0,
    {{push, Data}, 1+Size, Script1};
decode_op(<<Op, Script0/binary>>) when Op == ?OP_PUSHDATA1;
                                       Op == ?OP_PUSHDATA2;
                                       Op == ?OP_PUSHDATA4 ->
    SizeBytes = case Op of
                    ?OP_PUSHDATA1 -> 1;
                    ?OP_PUSHDATA2 -> 2;
                    ?OP_PUSHDATA4 -> 4
                end, 
    SizeBits = SizeBytes * 8,
    <<Size:SizeBits/little, Script1/binary>> = Script0,
    <<Data:Size/binary, Script2/binary>> = Script1,
    {{push, Data}, 1+SizeBytes+Size, Script2};
decode_op(<<OpCode, Script/binary>>) ->
    Op = case OpCode of
             ?OP_TRUE                -> {push, true};
             ?OP_FALSE               -> {push, false};
             ?OP_1NEGATE             -> {push, -1};
             _ when ?OP_N(OpCode)    -> {push, OpCode-?OP_1+1};
             ?OP_NOP                 -> nop;
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
             ?OP_LEFT                -> leftof;
             ?OP_RIGHT               -> rightof;
             ?OP_SIZE                -> size;
             ?OP_INVERT              -> invert;
             ?OP_AND                 -> bitwiseand;
             ?OP_OR                  -> bitwiseor;
             ?OP_XOR                 -> bitwisexor;
             ?OP_EQUAL               -> equal;
             ?OP_EQUALVERIFY         -> equalverify;
             ?OP_1ADD                -> increment;
             ?OP_1SUB                -> decrement;
             ?OP_2MUL                -> double;
             ?OP_2DIV                -> halve;
             ?OP_NEGATE              -> negate;
             ?OP_ABS                 -> absolutevalue;
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
             ?OP_MIN                 -> minimum;
             ?OP_MAX                 -> maximum;
             ?OP_WITHIN              -> within;
             ?OP_RIPEMD160           -> ripemd160;
             ?OP_SHA1                -> sha1;
             ?OP_SHA256              -> sha256;
             ?OP_HASH160             -> hash160;
             ?OP_HASH256             -> hash256;
             ?OP_CHECKSIG            -> checksignature;
             ?OP_CHECKSIGVERIFY      -> checksignatureverify;
             ?OP_CHECKMULTISIG       -> checkmultisignature;
             ?OP_CHECKMULTISIGVERIFY -> checkmultisignatureverify
         end,
    {Op, 1, Script}.
