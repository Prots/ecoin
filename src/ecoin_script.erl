-module(script).

-include("ecoin.hrl").

-export([genesis_raw/0,
         genesis/0,
         genesis/1]).

-export([new_env/1,
         new_env/3,
         run/1,
         run/6,
         pp/1]).

-export([encode/1,
         encode_op/1,
         encode_value/1,
         encode_bool/1,
         decode_bool/1,
         encode_varint/1,
         decode_varint/1]).

-record(env, {
          tx         :: #tx{}      | undefined,
          index      :: uinteger() | undefined,
          script     :: binary()
         }).

-type item()  :: boolean() | integer() | binary().
-type items() :: item() | [item()].

%% @doc Return the genesis scripts as binary
-spec genesis_raw() -> {script_raw(), script_raw()}.
genesis_raw() ->
    {SigScript, PKScript} = genesis(),
    {encode(SigScript), encode(PKScript)}.

%% @doc Return the genesis scripts
-spec genesis() -> {script(), script()}.
genesis() -> {genesis(sig_script), genesis(pk_script)}.

%% @doc Return the components of the genesis block
-spec genesis(script_enum()) -> script();
             (psz_timestamp) -> binary();
             (public_key)    -> public_key().
genesis(pk_script) ->
    [{push, genesis(public_key)}, checksig];
genesis(sig_script) ->
    [{push, 486604799}, {push, 4}, {push, genesis(psz_timestamp)}];
genesis(psz_timestamp) ->
    <<"The Times 03/Jan/2009 Chancellor on"
      " brink of second bailout for banks">>;
genesis(public_key) ->
    <<16#04,
      16#678AFDB0FE5548271967F1A67130B710:128, % x 256 bits
      16#5CD6A828E03909A67962E0EA1F61DEB6:128,
      16#49F6BC3F4CEF38C4F35504E51EC112DE:128, % y 256 bits
      16#5C384DF7BA0B8D578A4C702B6BF11D5F:128>>.

%% @doc Create an empty env to run a pk_script
-spec new_env(binary()) -> #env{}.
new_env(Script) -> #env{tx = undefined, index = undefined, script = Script}.

%% @doc Create a new environment to run a script in
-spec new_env(#tx{}, uinteger(), binary()) -> #env{}.
new_env(Tx, Index, Script) -> #env{tx = Tx, index = Index, script = Script}.

%% @doc Run a script in an environment
run(Env) -> run(Env, stack:new(), stack:new(), 0, 0, []).

run(#env{script = Script}, Primary, Secondary, PC, _, _)
  when PC == byte_size(Script) ->
    {Primary, Secondary};
run(Env, Primary, Secondary, PC, CS, IfStack) ->
    Op = binary:at(Env#env.script, PC),
    case Op of
        ?OP_ENDIF ->
            run(Env, Primary, Secondary, PC, CS, tl(IfStack));
        ?OP_IF ->
            {Bool, Primary1} = pop_bool(Primary),
            run(Env, Primary1, Secondary, PC+1, CS, [Bool|IfStack]);
        ?OP_NOTIF ->
            {Bool, Primary1} = pop_bool(Primary),
            run(Env, Primary1, Secondary, PC+1, CS, [not Bool|IfStack]);
        ?OP_ELSE ->
            IfStack1 = [not hd(IfStack)|tl(IfStack)],
            run(Env, Primary, Secondary, PC+1, CS, IfStack1);
        ?OP_NOP ->
            run(Env, Primary, Secondary, PC+1, CS, IfStack);
        _ ->
            case should_run(IfStack) of
                true  -> do_run(Env, Primary, Secondary, PC, CS, IfStack, Op);
                false -> skip(Env, Primary, Secondary, PC, CS, IfStack)
            end
    end.

do_run(Env, Primary, Secondary, PC, CS, IfStack, Op) ->
    case Op of
        ?OP_CODESEPARATOR ->
            run(Env, Primary, Secondary, PC+1, PC, IfStack);
        ?OP_RETURN ->
            throw({invalid, return});
        ?OP_TOALTSTACK ->
            {Head, Primary1} = stack:pop(Primary),
            run(Env, Primary1, stack:push(Secondary, Head),
                PC + 1, CS, IfStack);
        ?OP_FROMALTSTACK ->
            {Head, Secondary1} = stack:pop(Secondary),
            run(Env, stack:push(Primary, Head), Secondary1, PC+1, CS, IfStack);
        _ ->
            {Primary1, PC1} = stack_op(Env, Primary, PC, CS, Op),
            run(Env, Primary1, Secondary, PC1, CS, IfStack)
    end.

%% @doc Do a push operation
-spec push_op(binary(), stack:stack(), uinteger(), byte()) ->
    {stack:stack(), uinteger()}.
push_op(Script, Primary, PC, Size) when ?OP_PUSHDATA(Size) ->
    {push(Primary, binary:part(Script, PC, Size)), PC + Size};
push_op(Script, Primary, PC, ?OP_PUSHDATA1) ->
    Size = binary:at(Script, PC),
    Start = PC + 1,
    {push(Primary, binary:part(Script, Start, Size)), Start + Size};
push_op(Script, Primary, PC, ?OP_PUSHDATA2) ->
    <<Size:16/little>> = binary:part(Script, PC, 2),
    Start = PC + 2,
    {push(Primary, binary:part(Script, Start, Size)), Start + Size};
push_op(Script, Primary, PC, ?OP_PUSHDATA4) ->
    <<Size:32/little>> = binary:part(Script, PC, 4),
    Start = PC + 4,
    {push(Primary, binary:part(Script, Start, Size)), Start + Size}.

%% @doc Do a stack related operation
-spec stack_op(#env{}, stack:stack(), uinteger(), uinteger(), byte()) ->
    {stack:stack(), uinteger()}.
stack_op(#env{script = Script} = Env, Primary, PC, CS, Op) ->
    case Op > 0 andalso Op =< ?OP_PUSHDATA4 of
        true ->
            push_op(Script, Primary, PC, Op);
        false ->
            {case Op of
                 ?OP_CHECKSIG ->
                     op_checksig(Env, Primary, CS);
                 ?OP_CHECKSIGVERIFY ->
                     Primary0 = op_checksig(Env, Primary, CS),
                     stack_op(Primary0, ?OP_VERIFY);
                 ?OP_CHECKMULTISIG ->
                     op_checkmultisig(Env, Primary, CS);
                 ?OP_CHECKMULTISIGVERIFY ->
                     Primary0 = op_checkmultisig(Env, Primary, CS),
                     stack_op(Primary0, ?OP_VERIFY);
                 PureStackOp ->
                     stack_op(Primary, PureStackOp)
             end, PC + 1}
    end.

skip(#env{script = Script} = Env, Primary, Secondary, PC, CS, IfStack) ->
    PC1 = case binary:at(Script, PC) of
              Size when ?OP_PUSHDATA(Size) ->
                  PC + 1 + Size;
              ?OP_PUSHDATA1 ->
                  Size = binary:at(Script, PC+1),
                  PC + 2 + Size;
              ?OP_PUSHDATA2 ->
                  <<Size:16/little>> = binary:part(Script, PC+1, 2),
                  PC + 3 + Size;
              ?OP_PUSHDATA4 ->
                  <<Size:32/little>> = binary:part(Script, PC+1, 4),
                  PC + 5 + Size;
              _ ->
                  PC + 1
          end,
    run(Env, Primary, Secondary, PC1, CS, IfStack).

should_run([]) -> true;
should_run([Bool | IfStack]) -> Bool andalso should_run(IfStack).

%% @doc Pop the head of the stack and decode as boolean
-spec pop_bool(stack:stack()) -> {boolean(), stack:stack()}.
pop_bool(Stack) ->
    {Head, Stack1} = stack:pop(Stack),
    {decode_bool(Head), Stack1}.

%% @doc Run the checksignature operation in the current environment
-spec op_checksig(#env{}, stack:stack(), uinteger()) -> stack:stack().
op_checksig(#env{tx     = Tx,
                 index  = Index,
                 script = Script}, Primary, CodeSep) ->
    {[PublicKey, Signature], Primary1} = stack:pop_n(Primary, 2),
    AllButLast = byte_size(Signature) - 1,
    <<Signature1:AllButLast, HashTypeByte>> = Signature,
    SubScript = subscript(Script, Index, CodeSep),
    TxCopy = txcopy(Tx, Index, SubScript),
    Hash = hash(TxCopy, Index, HashTypeByte),
    Res = crypto:verify(ecdsa, sha256, Hash, Signature1, PublicKey),
    push(Primary1, Res).

%% @doc Run the checkmultisignature operation in the current environment
-spec op_checkmultisig(#env{}, stack:stack(), uinteger()) -> stack:stack().
op_checkmultisig(#env{}, _Primary, _Index) ->
    not_implemented.

%% @doc Given a transaction (txcopy) an input index and a hashtype
%%      compute the hash used for signature verification
-spec hash(#tx{}, uinteger(), byte()) -> binary().
hash(Tx, Index, HashType) ->
    Tx1 = case HashType band 31 of
              0               -> Tx;
              ?SIGHASH_ALL    -> Tx;
              ?SIGHASH_NONE   -> sighash_none(Tx, Index);
              ?SIGHASH_SINGLE -> sighash_single(Tx, Index)
          end,
    Tx2 = case HashType band ?SIGHASH_ANYONECANPAY == ?SIGHASH_ANYONECANPAY of
              true  -> sighash_anyonecanpay(Tx1, Index);
              false -> Tx1
          end,
    ecoin_crypto:hash256([ecoin_tx:encode(Tx2), <<HashType:32/little>>]).

%% @doc Transform a transaction (txcopy) according to the SIGHASH_NONE procedure
-spec sighash_none(#tx{}, uinteger()) -> #tx{}.
sighash_none(#tx{tx_in = Inputs} = Tx, TopIndex) ->
    SigHashNone = fun (Index, Input) when Index == TopIndex -> Input;
                      (_, Input) -> Input#tx_in{sequence = 0}
                  end,
    Tx#tx{tx_in  = array:map(SigHashNone, Inputs),
          tx_out = array:new(0)}.

%% @doc Transform a transaction (txcopy) according to the SIGHASH_SINGLE procedure
-spec sighash_single(#tx{}, uinteger()) -> #tx{}.
sighash_single(#tx{tx_out = Outputs} = Tx, TopIndex) ->
    {NewOutputList, _} = lists:split(TopIndex, array:to_list(Outputs)),
    Outputs1 = array:from_list(NewOutputList),
    SigHashSingle = fun (Index, Output) when Index == TopIndex ->
                            Output;
                        (_, Output) ->
                            Output#tx_out{value = -1, pk_script = <<>>}
                    end,
    Tx#tx{tx_out = array:map(SigHashSingle, Outputs1)}.

%% @doc Transform a transaction (txcopy) according to the SIGHASH_ANYONECANPAY
%%      procedure.
-spec sighash_anyonecanpay(#tx{}, uinteger()) -> #tx{}.
sighash_anyonecanpay(#tx{tx_in = Inputs} = Tx, Index) ->
    Tx#tx{tx_in = array:set(0, array:get(Index, Inputs), array:new(1))}.

%% @doc Create a subscript given a raw script, a signature and the index to the
%%      last code separator
-spec subscript(binary(), binary(), uinteger()) -> binary().
subscript(Script, Signature, CodeSep) ->
    SubScript = binary:part(Script, CodeSep, byte_size(Script)),
    iolist_to_binary(normalize_subscript(SubScript, Signature)).

%% @doc Create a new transaction from an old one where all the
%%      input scripts are set to an empty script except for the one
%%      with the given index. That one is set to the given subscript.
-spec txcopy(#tx{}, uinteger(), binary()) -> #tx{}.
txcopy(#tx{tx_in = Inputs} = Tx, IndexTop, SubScript) ->
    SetSubScript = fun (Index, Input) when Index == IndexTop ->
                           Input#tx_in{sig_script = SubScript};
                       (_, Input) ->
                           Input#tx_in{sig_script = <<>>}
                   end,
    Tx#tx{tx_in = array:map(SetSubScript, Inputs)}.

%% @doc Normalize subscript.
%%      Remove all traces of the signature and all code separators
-spec normalize_subscript(binary(), binary()) -> iolist().
normalize_subscript(<<>>, _) -> [];
normalize_subscript(<<Size, Signature:Size/binary,
                      SubScript/binary>>,Signature) when ?OP_PUSHDATA(Size) ->
    normalize_subscript(SubScript, Signature);
normalize_subscript(<<Size, Data:Size/binary,
                      SubScript/binary>>, Signature) when ?OP_PUSHDATA(Size) ->
    [Size, Data | normalize_subscript(SubScript, Signature)];
normalize_subscript(<<?OP_PUSHDATA1, Size, Signature:Size/binary,
                      SubScript/binary>>, Signature) ->
    normalize_subscript(SubScript, Signature);
normalize_subscript(<<?OP_PUSHDATA1, Size, Data:Size/binary,
                      SubScript/binary>>, Signature) ->
    [?OP_PUSHDATA1, Size, Data | normalize_subscript(SubScript, Signature)];
normalize_subscript(<<?OP_PUSHDATA2, Size:16/little, Signature:Size/binary,
                      SubScript/binary>>, Signature) ->
    normalize_subscript(SubScript, Signature);
normalize_subscript(<<?OP_PUSHDATA2, Size:16/little, Data:Size/binary,
                      SubScript/binary>>, Signature) ->
    [?OP_PUSHDATA2, <<Size:16/little>>, Data
     | normalize_subscript(SubScript, Signature)];
normalize_subscript(<<?OP_PUSHDATA4, Size:32/little, Signature:Size/binary,
                      SubScript/binary>>, Signature) ->
    normalize_subscript(SubScript, Signature);
normalize_subscript(<<?OP_PUSHDATA4, Size:32/little, Data:Size/binary,
                      SubScript/binary>>, Signature) ->
    [?OP_PUSHDATA4, <<Size:32/little>>, Data
     | normalize_subscript(SubScript, Signature)];
normalize_subscript(<<?OP_CODESEPARATOR, SubScript/binary>>, Signature) ->
    normalize_subscript(SubScript, Signature);
normalize_subscript(<<Operation, SubScript>>, Signature) ->
    [Operation | normalize_subscript(SubScript, Signature)].

%% @doc Transform the stack with function F
%%      The function F needs to correctly unpack it's arguments and
%%      return a list of new items to put on the stack
pop_f(Stack0, Fun) ->
    {arity, NArgs} = erlang:fun_info(Fun, arity),
    try stack:pop_n(Stack0, NArgs) of
        {Args, Stack1} ->
            case apply(Fun, Args) of
                Result when is_list(Result) ->
                    EncodedResult = lists:map(fun encode_value/1, Result),
                    stack:push_n(Stack1, EncodedResult);
                Result ->
                    push(Stack1, Result)
            end
    catch
        throw:stack_empty -> throw({invalid, too_few_arguments})
    end.

push(Stack, Data) ->
    stack:push(Stack, encode_value(Data)).

type_fun(Type, F) ->
    {arity, N} = erlang:fun_info(F, arity),
    Decode = case Type of
                 bool   -> fun decode_bool/1;
                 varint -> fun decode_varint/1
             end,
    type_fun(Decode, N, F).

%% @doc Given an arity(1, 2 or 3), a decode function and the function
%%      construct a new function that decodes it's argument and pipes
%%      the them to the given function.
-spec type_fun(fun ((binary()) -> item()), 1..3, fun ((...) -> items())) ->
          fun ((...) -> items()).
type_fun(Decode, 1, F) -> fun (A)       -> F(Decode(A)) end;
type_fun(Decode, 2, F) -> fun (A, B)    -> F(Decode(B), Decode(A)) end;
type_fun(Decode, 3, F) -> fun (A, B, C) -> F(Decode(C), Decode(B), Decode(A)) end.

%% @doc Return a function that decodes it's arguments as booleans.
%%      The given function should return a list of items.
%%      Works for arity 1, 2 and 3.
-spec bool_f(fun ((...) -> items())) -> fun ((...) -> items()).
bool_f(F) -> type_fun(fun decode_bool/1, F).

%% @doc Return a function that decodes it's arguments as varints.
%%      The given function should return a list of items.
%%      Works for functions of arity 1, 2 and 3.
-spec int_f(fun ((...) -> items())) -> fun ((...) -> items()).
int_f(F) -> type_fun(fun decode_varint/1, F).

%% @doc Return a function given the input hash algorithm
-spec crypto_f(sha1 | sha256 | ripemd160) -> fun ((binary()) -> binary()).
crypto_f(HashAlgo) -> fun(A) -> crypto:hash(HashAlgo, A) end.

%% @doc Run the next operation in the script, returning a new state
-spec stack_op(stack:stack(), byte()) -> stack:stack().
stack_op(S, ?OP_VERIFY) ->
    Verify = fun (true)  -> true;
                 (false) -> throw({invalid, verify})
             end,
    pop_f(S, bool_f(Verify));
stack_op(S, ?OP_IFDUP) ->
    IfDuplicate = fun (B) -> case decode_bool(B) of
                                 true  -> [B, B];
                                 false -> B
                             end
                  end,
    pop_f(S, IfDuplicate);
stack_op(S, ?OP_DEPTH) -> push(S, stack:size(S));
stack_op(S, {drop, N}) -> element(2, stack:pop_n(S, N));
stack_op(S, ?OP_DROP) -> stack_op(S, {drop, 1});
stack_op(S, ?OP_2DROP) -> stack_op(S, {drop, 2});
stack_op(S, {duplicate, N}) -> stack:push_n(S, stack:peek_n(S, N));
stack_op(S, ?OP_DUP) -> stack_op(S, {duplicate, 1});
stack_op(S, ?OP_2DUP) -> stack_op(S, {duplicate, 2});
stack_op(S, ?OP_3DUP) -> stack_op(S, {duplicate, 3});
stack_op(S, ?OP_NIP) -> element(2, stack:pop_ix(S, 2));
stack_op(S, {over, N}) -> stack:push_n(S, stack:peek_part(S, N*1, N));
stack_op(S, ?OP_OVER) -> stack_op(S, {over, 1});
stack_op(S, ?OP_2OVER) -> stack_op(S, {over, 2});
stack_op(S0, ?OP_PICK) ->
    {N, S1} = stack:pop(S0),
    stack:push(stack:peek_ix(decode_varint(N), S1), S1);
stack_op(S0, ?OP_ROLL) ->
    {N, S1} = stack:pop(S0),
    {I, S2} = stack:pop_ix(S1, decode_varint(N)),
    stack:push(I, S2);
stack_op(S0, {rotate, N}) ->
    {F, S1} = stack:pop_n(S0, N*3),
    {T1, T2} = lists:split(N*2, F),
    stack:push_n(S1, T2++T1);
stack_op(S, ?OP_ROT) -> stack_op(S, {rotate, 1});
stack_op(S, ?OP_2ROT) -> stack_op(S, {rotate, 2});
stack_op(S0, {swap, N}) ->
    {F, S1} = stack:pop_n(S0, N*2),
    {T1, T2} = lists:split(N, F),
    stack:push_n(S1, T2++T1);
stack_op(S, ?OP_SWAP) -> stack_op(S, {swap, 1});
stack_op(S, ?OP_2SWAP) -> stack_op(S, {swap, 2});
stack_op(S, ?OP_TUCK) -> pop_f(S, fun (A,B) -> [A, B, A] end);
stack_op(_S, ?OP_CAT) -> throw({invalid, {disabled, concatenate}});
    %Concatenate = fun (A, B) -> <<B/binary, A/binary>> end,
    %pop_f(S, Concatenate);
stack_op(_S, ?OP_SUBSTR) -> throw({invalid, {disabled, substring}});
    %SubString = fun (Size, Begin, Binary) ->
                    %binary:part(Binary,
                                %decode_varint(Begin),
                                %decode_varint(Size))
                %end,
    %pop_f(S, SubString);
stack_op(_S, ?OP_LEFT) -> throw({invalid, {disabled, leftof}});
    %LeftOf = fun (Index, Binary) ->
                 %binary:part(Binary, 0, decode_varint(Index))
             %end,
    %pop_f(S, LeftOf);
stack_op(_S, ?OP_RIGHT) -> throw({invalid, {disabled, rightof}});
    %RightOf = fun(Index, Binary) ->
                     %Ix = decode_varint(Index),
                     %binary:part(Binary, Ix+1, byte_size(Binary)-Ix-1)
              %end,
    %pop_f(S, RightOf);
stack_op(S, ?OP_SIZE) -> push(S, byte_size(stack:peek(S)));
stack_op(_S, ?OP_INVERT) -> throw({invalid, {disabled, invert}});
%pop_f(S, bitwise_f(fun erlang:'bnot'/1));
stack_op(_S, ?OP_AND) -> throw({invalid, {disabled, bitwiseand}});
%pop_f(S, bitwise_f(fun erlang:'band'/2));
stack_op(_S, ?OP_OR) -> throw({invalid, {disabled, bitwiseor}});
%pop_f(S, bitwise_f(fun erlang:'bor'/2));
stack_op(_S, ?OP_XOR) -> throw({invalid, {disabled, bitwisexor}});
%pop_f(S, bitwise_f(fun erlang:'bxor'/2));
stack_op(S, ?OP_EQUAL) -> pop_f(S, fun erlang:'=:='/2);
stack_op(S, ?OP_1ADD) -> pop_f(S, int_f(fun (A) -> A+1 end));
stack_op(S, ?OP_1SUB) -> pop_f(S, int_f(fun (A) -> A-1 end));
stack_op(_S, ?OP_2MUL) -> throw({invalid, {disabled, double}});
%pop_f(S, int_f(fun (A) -> A*2 end));
stack_op(_S, ?OP_2DIV) -> throw({invalid, {disabled, halve}});
%pop_f(S, int_f(fun (A) -> A div 2 end));
stack_op(S, ?OP_NEGATE) -> pop_f(S,int_f(fun (A) -> A*-1 end));
stack_op(S, ?OP_ABS) -> pop_f(S, int_f(fun abs/1));
stack_op(S, ?OP_NOT) -> pop_f(S, bool_f(fun erlang:'not'/1));
stack_op(S, ?OP_0NOTEQUAL) -> pop_f(S, bool_f(fun (A) -> A end));
stack_op(S, ?OP_ADD) -> pop_f(S, int_f(fun erlang:'+'/2));
stack_op(S, ?OP_SUB) -> pop_f(S, int_f(fun (A, B) -> B-A end));
stack_op(_S, ?OP_MUL) -> throw({invalid, {disabled, multiply}});
%pop_f(S, int_f(fun(A, B) -> A*B end));
stack_op(_S, ?OP_DIV) -> throw({invalid, {disabled, divide}});
%pop_f(S, int_f(fun (A, B) -> B div A end));
stack_op(_S, ?OP_MOD) -> throw({invalid, {disabled, modulus}});
%pop_f(S, int_f(fun (A, B) -> B rem A end));
stack_op(_S, ?OP_LSHIFT) -> throw({invalid, {disabled, leftshift}});
%pop_f(S, int_f(fun(A, B) -> sign(A)*(abs(A) bsl B) end));
stack_op(_S, ?OP_RSHIFT) -> throw({invalid, {disabled, rightshift}});
%pop_f(S, int_f(fun(A, B) -> sign(A)*(abs(A) bsr B) end));
stack_op(S, ?OP_BOOLAND) -> pop_f(S, bool_f(fun (A, B) -> A andalso B end));
stack_op(S, ?OP_BOOLOR) -> pop_f(S, bool_f(fun (A, B) -> A orelse B end));
stack_op(S, ?OP_NUMEQUAL) -> pop_f(S, int_f(fun (A, B) -> A == B end));
stack_op(S, ?OP_NUMNOTEQUAL) -> pop_f(S, int_f(fun (A, B) -> A /= B end));
stack_op(S, ?OP_NUMEQUALVERIFY) -> stack_op(stack_op(S, numequal), verify);
stack_op(S, ?OP_LESSTHAN) -> pop_f(S, int_f(fun (A, B) -> B < A end));
stack_op(S, ?OP_LESSTHANOREQUAL) -> pop_f(S, int_f(fun (A, B) -> B =< A end));
stack_op(S, ?OP_GREATERTHAN) -> pop_f(S, int_f(fun (A, B) -> B > A end));
stack_op(S, ?OP_GREATERTHANOREQUAL) ->
    pop_f(S, int_f(fun (A, B) -> B >= A end));
stack_op(S, ?OP_MIN) -> pop_f(S, int_f(fun (A, B) -> min(A, B) end));
stack_op(S, ?OP_MAX) -> pop_f(S, int_f(fun (A, B) -> max(A, B) end));
stack_op(S, ?OP_WITHIN) ->
    pop_f(S, int_f(fun (Max, Min, X) -> X >= Min andalso X < Max end));
stack_op(S, ?OP_RIPEMD160) -> pop_f(S, crypto_f(ripemd160));
stack_op(S, ?OP_SHA1) -> pop_f(S, crypto_f(sha1));
stack_op(S, ?OP_SHA256) -> pop_f(S, crypto_f(sha256));
stack_op(S, ?OP_HASH160) -> pop_f(S, fun ecoin_crypto:hash160/1);
stack_op(S, ?OP_HASH256) -> pop_f(S, fun ecoin_crypto:hash256/1).

%% @doc Return the sign
%-spec sign(integer()) -> integer().
%sign(Integer) when Integer < 0 -> -1;
%sign(_)                        ->  1.

%bitwise_f(F) ->
    %{arity, N} = erlang:fun_info(F, arity),
    %case N of
        %1 -> bitwise1_f(F);
        %2 -> bitwise2_f(F)
    %end.

%bitwise1_f(F) ->
    %fun(A) ->
            %Size = bit_size(A),
            %<<Int:Size>> = A,
            %<<(F(Int)):Size>>
    %end.

%bitwise2_f(F) ->
    %fun(A, B) ->
            %Size = bit_size(A),
            %{<<AInt:Size>>, <<BInt:Size>>} = {A, B},
            %<<(F(BInt, AInt)):Size>>
    %end.

%% @doc Encode a value
-spec encode_value(boolean() | integer() | binary()) -> binary().
encode_value(Boolean) when is_boolean(Boolean) -> encode_bool(Boolean);
encode_value(Integer) when is_integer(Integer) -> encode_varint(Integer);
encode_value(Binary)  when is_binary(Binary)   -> Binary.

%% @doc Encode a bool as a binary
-spec encode_bool(boolean()) -> binary().
encode_bool(true)  -> <<1>>;
encode_bool(false) -> <<>>.

%% @doc Encode a variable size little endian integer
-spec encode_varint(integer()) -> binary().
encode_varint(0)                -> <<>>;
encode_varint(Int) when Int < 0 -> encode_varint(abs(Int), negative);
encode_varint(Int)              -> encode_varint(Int, positive).

-spec encode_varint(uinteger(), positive | negative) -> binary().
encode_varint(UInt, Sign) ->
    Bin0 = binary:encode_unsigned(UInt, little),
    %% Pad with an extra byte if it overflows
    LastByte = binary:last(Bin0),
    case LastByte band 16#80 == 16#80 of
        true ->
            case Sign of
                positive -> <<Bin0/binary, 0>>;
                negative -> <<Bin0/binary, 16#80>>
            end;
        false ->
            case Sign of
                positive -> Bin0;
                negative ->
                    AllButLast = byte_size(Bin0)-1,
                    <<AllButLastByte:AllButLast/binary, LastByte>> = Bin0,
                    <<AllButLastByte/binary, (LastByte+16#80)>>
            end
    end.

%% @doc Decode a binary as a bool
-spec decode_bool(binary()) -> binary().
decode_bool(Data) -> decode_varint(Data) /= 0.

%% @doc Decode a variable size little endian integer
%%      Most significant bit in last byte encodes sign,
%%      if the number overflows, add an extra byte in front of it
-spec decode_varint(binary()) -> integer().
decode_varint(<<>>) -> 0;
decode_varint(Bin0) when byte_size(Bin0) =< 4->
    AllButLast = byte_size(Bin0)-1,
    <<AllButLastByte:AllButLast/binary, LastByte0>> = Bin0,
    {Sign, LastByte1}= case LastByte0 band 16#80 == 16#80 of
                           true  -> {-1, LastByte0 - 16#80};
                           false -> {1,  LastByte0}
                       end,
    Sign * binary:decode_unsigned(<<AllButLastByte/binary, LastByte1>>, little);
decode_varint(_) -> {invalid, overflow}.

%% @doc Encode a script
-spec encode(script()) -> script_raw().
encode(Script) -> iolist_to_binary(lists:map(fun encode_op/1, Script)).

%% @doc Encode a script operation
-spec encode_op(script_operation()) -> iodata().
encode_op(Bool) when is_boolean(Bool) -> encode_op(encode_bool(Bool));
encode_op(Int) when is_integer(Int), Int > -1, Int < 16 -> encode_varint(Int);
encode_op(Bin) when is_binary(Bin), byte_size(Bin) < ?MAX_ELEMENT_SIZE ->
    encode_pushdata(byte_size(Bin), Bin);
encode_op('if') -> ?OP_IF;
encode_op(notif) -> ?OP_NOTIF;
encode_op(else) -> ?OP_ELSE;
encode_op(endif) -> ?OP_ENDIF;
encode_op(nop) -> ?OP_NOP;
encode_op(verify) -> ?OP_VERIF;
encode_op(return) -> ?OP_RETURN;
encode_op(depth) -> ?OP_DEPTH;
encode_op(toaltstack) -> ?OP_TOALTSTACK;
encode_op(fromaltstack) -> ?OP_FROMALTSTACK;
encode_op(nip) -> ?OP_NIP;
encode_op(pick) -> ?OP_PICK;
encode_op(roll) -> ?OP_ROLL;
encode_op(tuck) -> ?OP_TUCK;
encode_op(ifdup) -> ?OP_IFDUP;
encode_op({dup, 1}) -> ?OP_DUP;
encode_op({dup, 2}) -> ?OP_2DUP;
encode_op({dup, 3}) -> ?OP_3DUP;
encode_op({drop, 1}) -> ?OP_DROP;
encode_op({drop, 2}) -> ?OP_2DROP;
encode_op({over, 1}) -> ?OP_OVER;
encode_op({over, 2}) -> ?OP_2OVER;
encode_op({rot, 1}) -> ?OP_ROT;
encode_op({rot, 2}) -> ?OP_2ROT;
encode_op({swap, 1}) -> ?OP_SWAP;
encode_op({swap, 2}) -> ?OP_2SWAP;
encode_op(cat) -> ?OP_CAT;
encode_op(substr) -> ?OP_SUBSTR;
encode_op(left) -> ?OP_LEFT;
encode_op(right) -> ?OP_RIGHT;
encode_op(size) -> ?OP_SIZE;
encode_op(invert) -> ?OP_INVERT;
encode_op('and') -> ?OP_AND;
encode_op('or') -> ?OP_OR;
encode_op('xor') -> ?OP_XOR;
encode_op(equal) -> ?OP_EQUAL;
encode_op(equalverify) -> ?OP_EQUALVERIFY;
encode_op(add1) -> ?OP_1ADD;
encode_op(sub1) -> ?OP_1SUB;
encode_op(add) -> ?OP_ADD;
encode_op(sub) -> ?OP_SUB;
encode_op(mul2) -> ?OP_2MUL;
encode_op(div2) -> ?OP_2DIV;
encode_op(lshift) -> ?OP_LSHIFT;
encode_op(rshift) -> ?OP_RSHIFT;
encode_op(mul) -> ?OP_MUL;
encode_op('div') -> ?OP_DIV;
encode_op(mod) -> ?OP_MOD;
encode_op(negate) -> ?OP_NEGATE;
encode_op(abs) -> ?OP_ABS;
encode_op(notequal0) -> ?OP_0NOTEQUAL;
encode_op('not') -> ?OP_NOT;
encode_op(booland) -> ?OP_BOOLAND;
encode_op(boolor) -> ?OP_BOOLOR;
encode_op(numequal) -> ?OP_NUMEQUAL;
encode_op(numnotequal) -> ?OP_NUMNOTEQUAL;
encode_op(lessthan) -> ?OP_LESSTHAN;
encode_op(lessthanorequal) -> ?OP_LESSTHANOREQUAL;
encode_op(greaterthan) -> ?OP_GREATERTHAN;
encode_op(greaterthanorequal) -> ?OP_GREATERTHANOREQUAL;
encode_op(min) -> ?OP_MIN;
encode_op(max) -> ?OP_MAX;
encode_op(witin) -> ?OP_WITHIN;
encode_op(codeseparator) -> ?OP_CODESEPARATOR;
encode_op(ripemd160) -> ?OP_RIPEMD160;
encode_op(sha1) -> ?OP_SHA1;
encode_op(sha256) -> ?OP_SHA256;
encode_op(hash160) -> ?OP_HASH160;
encode_op(hash256) -> ?OP_HASH256;
encode_op(checksig) -> ?OP_CHECKSIG;
encode_op(checksigverify) -> ?OP_CHECKSIGVERIFY;
encode_op(checkmultisig) -> ?OP_CHECKMULTISIG;
encode_op(checkmultisigverify) -> ?OP_CHECKMULTISIGVERIFY.

%% @doc Encode a push data operation into one
%%      of the available push op codes
-spec encode_pushdata(uinteger(), binary()) -> binary().
encode_pushdata(Size, Data) when Size < ?OP_PUSHDATA1 -> <<Size, Data/binary>>;
encode_pushdata(Size, Data) when Size =< 16#FF ->
    <<?OP_PUSHDATA1, Size, Data/binary>>;
encode_pushdata(Size, Data) when Size =< 16#FFFF ->
    <<?OP_PUSHDATA2, Size:16/little, Data/binary>>;
encode_pushdata(Size, Data) when Size =< 16#FFFFFFFF ->
    <<?OP_PUSHDATA4, Size:32/little, Data/binary>>.

%% @doc Pretty print a script
-spec pp(binary()) -> iodata().
pp(<<>>) -> <<>>;
pp(<<Size, Data:Size/binary, Script/binary>>) when ?OP_PUSHDATA(Size) ->
    [<<"PUSH0: ", Data/binary>>,
     $\n,
     pp(Script)];
pp(<<Op, Script/binary>>) when Op == ?OP_PUSHDATA1;
                                      Op == ?OP_PUSHDATA2;
                                      Op == ?OP_PUSHDATA4 ->
    SizeBytes = case Op of
                    ?OP_PUSHDATA1 -> 1;
                    ?OP_PUSHDATA2 -> 2;
                    ?OP_PUSHDATA4 -> 4
                end,
    SizeBits = SizeBytes * 8,
    <<Size:SizeBits/little, Data:Size/binary, Script1/binary>> = Script,
    [<<"PUSH", (integer_to_binary(SizeBytes))/binary, Data/binary>>,
     $\n,
     pp(Script1)];
pp(<<Op, Script/binary>>) ->
    [case Op of
         ?OP_IF                  -> <<"IF">>;
         ?OP_NOTIF               -> <<"NOT IF">>;
         ?OP_ELSE                -> <<"ELSE">>;
         ?OP_ENDIF               -> <<"END IF">>;
         ?OP_TRUE                -> <<"PUSH: 1/TRUE">>;
         ?OP_FALSE               -> <<"PUSH: 0/FALSE">>;
         ?OP_1NEGATE             -> <<"PUSH: -1">>;
         _ when ?OP_N(Op)        -> <<"PUSH:", (integer_to_binary(Op-?OP_1+1))/binary>>;
         ?OP_NOP                 -> <<"NO-OP">>;
         ?OP_VERIFY              -> <<"VERIFY">>;
         ?OP_RETURN              -> <<"RETURN">>;
         ?OP_TOALTSTACK          -> <<"TO SECONDARY">>;
         ?OP_FROMALTSTACK        -> <<"FROM SECONDARY">>;
         ?OP_IFDUP               -> <<"IF DUPLICATE">>;
         ?OP_DEPTH               -> <<"DEPTH">>;
         ?OP_DROP                -> <<"DROP: 1">>;
         ?OP_2DROP               -> <<"DROP: 2">>;
         ?OP_DUP                 -> <<"DUPLICATE: 1">>;
         ?OP_2DUP                -> <<"DUPLICATE: 2">>;
         ?OP_3DUP                -> <<"DUPLICATE: 3">>;
         ?OP_NIP                 -> <<"NIP">>;
         ?OP_OVER                -> <<"OVER: 1">>;
         ?OP_2OVER               -> <<"OVER: 2">>;
         ?OP_PICK                -> <<"PICK">>;
         ?OP_ROLL                -> <<"ROLL">>;
         ?OP_ROT                 -> <<"ROTATE: 1">>;
         ?OP_2ROT                -> <<"ROTATE: 2">>;
         ?OP_SWAP                -> <<"SWAP: 1">>;
         ?OP_2SWAP               -> <<"SWAP: 2">>;
         ?OP_TUCK                -> <<"TUCK">>;
         ?OP_CAT                 -> <<"CONCATENATE (disabled)">>;
         ?OP_SUBSTR              -> <<"SUBSTRING (disabled)">>;
         ?OP_LEFT                -> <<"LEFT OF (disabled)">>;
         ?OP_RIGHT               -> <<"RIGHT OF (disabled)">>;
         ?OP_SIZE                -> <<"SIZE">>;
         ?OP_INVERT              -> <<"INVERT (disabled)">>;
         ?OP_AND                 -> <<"BITWISE AND (disabled)">>;
         ?OP_OR                  -> <<"BITWISE OR (disabled)">>;
         ?OP_XOR                 -> <<"BITWISE XOR (disabled)">>;
         ?OP_EQUAL               -> <<"EQUAL">>;
         ?OP_EQUALVERIFY         -> <<"EQUAL THEN VERIFY">>;
         ?OP_1ADD                -> <<"INCREMENT">>;
         ?OP_1SUB                -> <<"DECREMENT">>;
         ?OP_2MUL                -> <<"DOUBLE (disabled)">>;
         ?OP_2DIV                -> <<"HALVE (disabled)">>;
         ?OP_NEGATE              -> <<"NEGATE">>;
         ?OP_ABS                 -> <<"ABSOLUTE VALUE">>;
         ?OP_NOT                 -> <<"NOT">>;
         ?OP_0NOTEQUAL           -> <<"NOT EQUAL 0">>;
         ?OP_ADD                 -> <<"ADD">>;
         ?OP_SUB                 -> <<"SUBTRACT">>;
         ?OP_MUL                 -> <<"MULTIPLY (disabled)">>;
         ?OP_DIV                 -> <<"DIVIDE (disabled)">>;
         ?OP_MOD                 -> <<"MODULUS (disalbed)">>;
         ?OP_LSHIFT              -> <<"LEFT SHIFT (disabled)">>;
         ?OP_RSHIFT              -> <<"RIGHT SHIFT (disabled)">>;
         ?OP_BOOLAND             -> <<"AND">>;
         ?OP_BOOLOR              -> <<"OR">>;
         ?OP_NUMEQUAL            -> <<"EQUAL NUM">>;
         ?OP_NUMNOTEQUAL         -> <<"NOT EQUAL NUM">>;
         ?OP_NUMEQUALVERIFY      -> <<"EQUAL NUM THEN VERIFY">>;
         ?OP_LESSTHAN            -> <<"LESS THAN">>;
         ?OP_LESSTHANOREQUAL     -> <<"LESS THAN OR EQUAL">>;
         ?OP_GREATERTHAN         -> <<"GREATER THAN">>;
         ?OP_GREATERTHANOREQUAL  -> <<"GREATER THAN OR EQUAL">>;
         ?OP_MIN                 -> <<"MINIMUM">>;
         ?OP_MAX                 -> <<"MAXIMUM">>;
         ?OP_WITHIN              -> <<"WITHIN">>;
         ?OP_RIPEMD160           -> <<"RIPEMD160">>;
         ?OP_SHA1                -> <<"SHA1">>;
         ?OP_SHA256              -> <<"SHA256">>;
         ?OP_HASH160             -> <<"HASH160">>;
         ?OP_HASH256             -> <<"HASH256">>;
         ?OP_CHECKSIG            -> <<"CHECK SIGNATURE">>;
         ?OP_CHECKSIGVERIFY      -> <<"CHECK SIGNATURE THEN VERIFY">>;
         ?OP_CHECKMULTISIG       -> <<"CHECK MULTISIGNATURE">>;
         ?OP_CHECKMULTISIGVERIFY -> <<"CHECK MULTISIGNATURE THEN VERIFY">>
     end, $\n,
     pp(Script)].
