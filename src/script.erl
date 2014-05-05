-module(script).

-export([run/2,
         run/3,
         pack/1,
         unpack/1]).

-include("ecoin.hrl").
-include("script.hrl").

-record(env, {
          stack    = [] :: list(binary()),
          altstack = [] :: list(binary()),
          tx            :: #tx{},
          index         :: non_neg_integer()
         }).

%% @doc Run a script validating transaction tx, input Index
run(Script, Tx, Index) ->
    run(Script, #env{tx=Tx, index=Index}).

%% @doc Run a script with given environment
run(Script, Env) ->
    lists:foldl(fun step/2, Env, Script).

%% @doc Pack a script
pack(Script) ->
    lists:map(fun pack_op/1, Script).

%% @doc Unpack a script
unpack(Binary) ->
    unpack(Binary, []).

%% @doc Unpack operations until no more is left
unpack(<<>>, Acc) ->
    lists:reverse(Acc);
unpack(Binary0, Acc) ->
    {Op, Binary1} = unpack_op(Binary0),
    unpack(Binary1, [Op|Acc]).

%% @doc Run a operation on a given environment
step(_, {error, Error}) ->
    {error, Error};
step(op_false, #env{stack=Stack} = Env) ->
    Env#env{stack=[pack_bool(false)|Stack]};
step(op_true, #env{stack=Stack} = Env) ->
    Env#env{stack=[pack_bool(true)|Stack]};
step(op_1negate, #env{stack=Stack} = Env) ->
    Env#env{stack=[pack_var_int(-1)|Stack]};
step({op_n, N}, #env{stack=Stack} = Env) ->
    Env#env{stack=[pack_var_int(N) | Stack]};
step({op_pushdata, Data}, #env{stack=Stack} = Env) ->
    Env#env{stack=[Data | Stack]};
step(op_nop, Env) ->
    Env;
step({op_if, True, False}, #env{stack=[Cond|Stack]} = Env) ->
    Branch = case unpack_bool(Cond) of
                 true  -> True;
                 false -> False
             end,
    run(Branch, Env#env{stack=Stack});
step({op_notif, False, True}, #env{stack=[Cond|Stack]} = Env) ->
    Branch = case unpack_bool(Cond) of
                 true  -> True;
                 false -> False
             end,
    run(Branch, Env#env{stack=Stack});
step(op_verify, #env{stack=[A|Stack]} = Env) ->
    case unpack_bool(A) of
        true  -> Env#env{stack=Stack};
        false -> {error, invalid}
    end;
step(op_return, _) ->
    {error, invalid};
step(op_toaltstack, #env{stack=[A|Stack], altstack=AltStack} = Env) ->
    Env#env{stack=Stack, altstack=[A|AltStack]};
step(op_fromaltstack, #env{stack=Stack, altstack=[A|AltStack]} = Env) ->
    Env#env{stack=[A|Stack], altstack=AltStack};
step(op_ifdup, #env{stack=[A|_] = Stack} = Env) ->
    case unpack_bool(A) of
        true  -> Env#env{stack=[A|Stack]};
        false -> Env
    end;
step(op_depth, #env{stack=Stack} = Env) ->
    Env#env{stack=[pack_var_int(length(Stack))|Stack]};
step({op_drop, N}, #env{stack=Stack} = Env) ->
    Env#env{stack=lists:nthtail(N, Stack)};
step({op_dup, N}, #env{stack=Stack} = Env) ->
    {Dups, _} = lists:split(N, Stack),
    Env#env{stack=Dups ++ Stack};
step(op_nip, #env{stack=[A, _|Stack]} = Env) ->
    Env#env{stack=[A|Stack]};
step({op_over, N}, #env{stack=Stack} = Env) ->
    Tail = lists:nthtail(N, Stack),
    {NewTop, _} = lists:split(N, Tail),
    Env#env{stack=NewTop ++ Stack};
step(op_pick, #env{stack=[N|Stack]} = Env) ->
    Nth = lists:nth(unpack_var_int(N)+1, Stack),
    Env#env{stack=[Nth|Stack]};
step(op_roll, #env{stack=[N|Stack]} = Env) ->
    {Front, [Nth|Tail]} = lists:split(unpack_var_int(N), Stack),
    Env#env{stack=[Nth|(Front++Tail)]};
step({op_rot, N}, #env{stack=Stack} = Env) ->
    {Front, Tail} = lists:split(N*3, Stack),
    {OldTop, NewTop} = lists:split(N*2, Front),
    Top = NewTop ++ OldTop,
    Env#env{stack=Top ++ Tail};
step({op_swap, N}, #env{stack=Stack} = Env) ->
    {Front, Tail} = lists:split(N*2, Stack),
    {OldTop, NewTop} = lists:split(N, Front),
    Top = NewTop ++ OldTop,
    Env#env{stack=Top ++ Tail};
step(op_tuck, #env{stack=[A,B|Stack]} = Env) ->
    Env#env{stack=[A, B, A|Stack]};
step(op_cat, #env{stack=[A, B|Stack]} = Env) ->
    Env#env{stack=[<<A/binary, B/binary>>|Stack]};
step(op_substr, #env{stack=[Size, Begin, A|Stack]} = Env) ->
    Env#env{stack=[binary:part(A, Begin, Size)|Stack]};
step(op_left, #env{stack=[Index, A|Stack]} = Env) ->
    Env#env{stack=[binary:part(A, 0, Index)|Stack]};
step(op_right, #env{stack=[Index, A|Stack]} = Env) ->
    Size = byte_size(A) - Index - 1,
    Env#env{stack=[binary:part(A, Index + 1, Size)|Stack]};
step(op_size, #env{stack=[A|Stack]} = Env) ->
    Env#env{stack=[pack_var_int(byte_size(A)), A|Stack]};
step(op_invert, #env{stack=[A|Stack]} = Env) ->
    Size = byte_size(A) * 8,
    <<Integer:Size>> = A,
    Env#env{stack=[<<bnot(Integer):Size>>|Stack]};
step(op_and, #env{stack=[A, B|Stack]} = Env) 
  when byte_size(A) == byte_size(B) ->
    Size = byte_size(A) * 8,
    {<<AInt:Size>>, <<BInt:Size>>} = {A, B},
    Env#env{stack=[<<(AInt band BInt):Size>>|Stack]};
step(op_or, #env{stack=[A, B|Stack]} = Env) 
  when byte_size(A) == byte_size(B) ->
    Size = byte_size(A) * 8,
    {<<AInt:Size>>, <<BInt:Size>>} = {A, B},
    Env#env{stack=[<<(AInt bor BInt):Size>>|Stack]};
step(op_xor, #env{stack=[A, B|Stack]} = Env) 
  when byte_size(A) == byte_size(B)->
    Size = byte_size(A) * 8,
    {<<AInt:Size>>, <<BInt:Size>>} = {A, B},
    Env#env{stack=[<<(AInt bxor BInt):Size>>|Stack]};
step(op_equal, #env{stack=[A, B|Stack]} = Env) ->
    Env#env{stack=[pack_bool(A =:= B)|Stack]};
step(op_equalverify, Env) ->
     step(op_verify, step(op_equal, Env));
step(op_1add, #env{stack=[A|Stack]} = Env) ->
    Env#env{stack=[pack_var_int(unpack_var_int(A) + 1)|Stack]};
step(op_1sub, #env{stack=[A|Stack]} = Env) ->
    Env#env{stack=[pack_var_int(unpack_var_int(A) - 1)|Stack]};
step(op_2mul, #env{stack=[A|Stack]} = Env) ->
    Env#env{stack=[pack_var_int(unpack_var_int(A) * 2)|Stack]};
step(op_2div, #env{stack=[A|Stack]} = Env) ->
    Env#env{stack=[pack_var_int(unpack_var_int(A) div 2)|Stack]};
step(op_negate, #env{stack=[A|Stack]} = Env) ->
    Env#env{stack=[pack_var_int(unpack_var_int(A) * -1)|Stack]};
step(op_abs, #env{stack=[A|Stack]} = Env) ->
    Env#env{stack=[pack_var_int(abs(unpack_var_int(A)))|Stack]};
step(op_not, #env{stack=[A|Stack]} = Env) ->
    Env#env{stack=[pack_bool(not(unpack_bool(A)))|Stack]};
step(op_0notequal, #env{stack=[A|Stack]} = Env) ->
    Env#env{stack=[pack_bool(unpack_bool(A))|Stack]};
step(op_add, #env{stack=[B, A|Stack]} = Env) ->
    Sum = unpack_var_int(B)  + unpack_var_int(A),
    Env#env{stack=[pack_var_int(Sum)|Stack]};
step(op_sub, #env{stack=[B, A|Stack]} = Env) ->
    Difference = unpack_var_int(A) - unpack_var_int(B),
    Env#env{stack=[pack_var_int(Difference)|Stack]};
step(op_mul, #env{stack=[B, A|Stack]} = Env) ->
    Product = unpack_var_int(A) * unpack_var_int(B),
    Env#env{stack=[pack_var_int(Product)|Stack]};
step(op_div, #env{stack=[B, A|Stack]} = Env) ->
    Quotient = unpack_var_int(A) div unpack_var_int(B),
    Env#env{stack=[pack_var_int(Quotient)|Stack]};
step(op_mod, #env{stack=[B, A|Stack]} = Env) ->
    Rem = unpack_var_int(A) rem unpack_var_int(B),
    Env#env{stack=[pack_var_int(Rem)|Stack]};
step(op_lshift, #env{stack=[B, A|Stack]} = Env) ->
    Sign = case unpack_var_int(A) < 0 of
               true  -> -1;
               false -> 1
           end,
    Env#env{stack=[pack_var_int((abs(A) bsl B) * Sign)|Stack]};
step(op_rshift, #env{stack=[B, A|Stack]} = Env) ->
    Sign = case unpack_var_int(A) < 0 of
               true  -> -1;
               false -> 1
           end,
    Env#env{stack=[pack_var_int((abs(A) bsr B) * Sign)|Stack]};
step(op_booland, #env{stack=[B, A|Stack]} = Env) ->
    And = unpack_bool(A) and unpack_bool(B),
    Env#env{stack=[pack_bool(And)|Stack]};
step(op_boolor, #env{stack=[B, A|Stack]} = Env) ->
    Or = unpack_bool(A) or unpack_bool(B),
    Env#env{stack=[pack_bool(Or)|Stack]};
step(op_numequal, #env{stack=[B, A|Stack]} = Env) ->
    Equal = unpack_var_int(A) =:= unpack_var_int(B),
    Env#env{stack=[pack_bool(Equal)|Stack]};
step(op_numequalverify, Env) ->
    step(op_verify, step(op_numequal, Env));
step(op_numnotequal, #env{stack=[B, A|Stack]} = Env) ->
    NotEqual = not(unpack_var_int(A) =:= unpack_var_int(B)),
    Env#env{stack=[pack_bool(NotEqual)|Stack]};
step(op_lessthan, #env{stack=[B, A|Stack]} = Env) ->
    LessThan = unpack_var_int(A) < unpack_var_int(B),
    Env#env{stack=[pack_bool(LessThan)|Stack]};
step(op_greaterthan, #env{stack=[B, A|Stack]} = Env) ->
    GreaterThan = unpack_var_int(A) > unpack_var_int(B),
    Env#env{stack=[pack_bool(GreaterThan)|Stack]};
step(op_lessthanorequal, #env{stack=[B, A|Stack]} = Env) ->
    LessThanOrEqual = unpack_var_int(A) =< unpack_var_int(B),
    Env#env{stack=[pack_bool(LessThanOrEqual)|

%% @doc Pack a bool as a binary
pack_bool(true)  -> pack_var_int(1);
pack_bool(false) -> pack_var_int(0).

%% @doc Unpack a binary as a bool
unpack_bool(Binary) ->
    case unpack_var_int(Binary) of
        0 -> false;
        _ -> true
    end.

%% @doc Pack a variable size little endian integer
pack_var_int(0) ->
    <<>>;
pack_var_int(Int0) ->
    Int1 = abs(Int0),
    Binary0 = binary:encode_unsigned(Int1, little),
    %% Pad with an extra byte if it overflows
    Binary1 = case binary:last(Binary0) bsr 7 of
                  0 -> Binary0;
                  1 -> <<Binary0/binary, 0>>
              end,
    case Int0 >= 0 of
        true  ->
           Binary1; 
        false ->
            Size = byte_size(Binary1) - 1,
            <<Binary2:Size/binary, MSB>> = Binary1,
            %% Set most significant bit
            SignedMSB = MSB bor 16#80,
            <<Binary2/binary, SignedMSB>>
    end.

%% @doc Unpack a variable size little endian integer
%%      Most significant bit in last byte encodes sign,
%%      if the number overflows, add an extra byte in front of it
unpack_var_int(<<>>) ->
    0;
unpack_var_int(Binary0) ->
    Size = byte_size(Binary0) - 1,
    <<Binary1:Size/binary, MSB>> = Binary0,
    {Sign, Binary2}= case MSB bsr 7 of
                         0 ->
                             {1, Binary1};
                         1 ->
                             UnsignedMSB = MSB band bnot(16#80),
                             {-1, <<Binary1/binary, UnsignedMSB>>}
                     end,
    Sign * binary:decode_unsigned(Binary2, little).

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

%% @doc Unpack a script operation
unpack_op(<<Size, Binary0/binary>>) when ?OP_PUSHDATA(Size) ->
    <<Data:Size/binary, Binary1/binary>> = Binary0,
    {{op_pushdata, Data}, Binary1};
unpack_op(<<Op, Binary0/binary>>) when Op == ?OP_PUSHDATA1;
                                       Op == ?OP_PUSHDATA2;
                                       Op == ?OP_PUSHDATA4 ->
    SizeBits = case Op of
                   ?OP_PUSHDATA1 -> 8;
                   ?OP_PUSHDATA2 -> 16;
                   ?OP_PUSHDATA4 -> 32
               end, 
    <<Size:SizeBits,    Binary1/binary>> = Binary0,
    <<Data:Size/binary, Binary2/binary>> = Binary1,
    {{op_pushdata, Data}, Binary2};
unpack_op(<<Op, Binary0/binary>>) when Op == ?OP_IF;
                                       Op == ?OP_NOTIF ->
    OpIf = case Op of
             ?OP_IF    -> op_if;
             ?OP_NOTIF -> op_notif
         end,
    {BranchA, BranchB, Binary1} = unpack_op_if(Binary0),
    {{OpIf, BranchA, BranchB}, Binary1};
unpack_op(<<OpCode, Binary0/binary>>) ->
    Op = case OpCode of
             ?OP_TRUE                -> op_true;
             ?OP_FALSE               -> op_false;
             ?OP_1NEGATE             -> op_1negate;
             _ when ?OP_N(OpCode)    -> {op_n, OpCode-?OP_1+1};
             ?OP_NOP                 -> op_nop;
             ?OP_ELSE                -> op_else;
             ?OP_ENDIF               -> op_endif;
             ?OP_VERIFY              -> op_verify;
             ?OP_RETURN              -> op_return;
             ?OP_TOALTSTACK          -> op_toaltstack;
             ?OP_FROMALTSTACK        -> op_fromaltstack;
             ?OP_IFDUP               -> op_ifdup;
             ?OP_DEPTH               -> op_depth;
             ?OP_DROP                -> {op_drop, 1};
             ?OP_2DROP               -> {op_drop, 2};
             ?OP_DUP                 -> {op_dup, 1};
             ?OP_2DUP                -> {op_dup, 2};
             ?OP_3DUP                -> {op_dup, 3};
             ?OP_NIP                 -> op_nip;
             ?OP_OVER                -> {op_over, 1};
             ?OP_2OVER               -> {op_over, 2};
             ?OP_PICK                -> op_pick;
             ?OP_ROLL                -> op_roll;
             ?OP_ROT                 -> {op_rot, 1};
             ?OP_2ROT                -> {op_rot, 2};
             ?OP_SWAP                -> {op_swap, 1};
             ?OP_2SWAP               -> {op_swap, 2};
             ?OP_TUCK                -> op_tuck;
             ?OP_CAT                 -> op_cat;
             ?OP_SUBSTR              -> op_substr;
             ?OP_LEFT                -> op_left;
             ?OP_RIGHT               -> op_right;
             ?OP_SIZE                -> op_size;
             ?OP_INVERT              -> op_invert;
             ?OP_AND                 -> op_and;
             ?OP_OR                  -> op_or;
             ?OP_XOR                 -> op_xor;
             ?OP_EQUAL               -> op_equal;
             ?OP_EQUALVERIFY         -> op_equalverify;
             ?OP_1ADD                -> op_1add;
             ?OP_1SUB                -> op_1sub;
             ?OP_2MUL                -> op_2mul;
             ?OP_2DIV                -> op_2div;
             ?OP_NEGATE              -> op_negate;
             ?OP_ABS                 -> op_abs;
             ?OP_NOT                 -> op_not;
             ?OP_0NOTEQUAL           -> op_0notequal;
             ?OP_ADD                 -> op_add;
             ?OP_SUB                 -> op_sub;
             ?OP_MUL                 -> op_mul;
             ?OP_DIV                 -> op_div;
             ?OP_MOD                 -> op_mod;
             ?OP_LSHIFT              -> op_lshift;
             ?OP_RSHIFT              -> op_rshift;
             ?OP_BOOLAND             -> op_booland;
             ?OP_BOOLOR              -> op_boolor;
             ?OP_NUMEQUAL            -> op_numequal;
             ?OP_NUMEQUALVERIFY      -> op_numequalverify;
             ?OP_NUMNOTEQUAL         -> op_numnotequal;
             ?OP_LESSTHAN            -> op_lessthan;
             ?OP_GREATERTHAN         -> op_greaterthan;
             ?OP_LESSTHANOREQUAL     -> op_lessthanorequal;
             ?OP_GREATERTHANOREQUAL  -> op_greaterthanorequal;
             ?OP_MIN                 -> op_min;
             ?OP_MAX                 -> op_max;
             ?OP_WITHIN              -> op_within;
             ?OP_RIPEMD160           -> op_ripemd160;
             ?OP_SHA1                -> op_sha1;
             ?OP_SHA256              -> op_sha256;
             ?OP_HASH160             -> op_hash160;
             ?OP_HASH256             -> op_hash256;
             ?OP_CODESEPARATOR       -> op_codeseparator;
             ?OP_CHECKSIG            -> op_checksig;
             ?OP_CHECKSIGVERIFY      -> op_checksigverify;
             ?OP_CHECKMULTISIG       -> op_checkmultisig;
             ?OP_CHECKMULTISIGVERIFY -> op_checkmultisigverify;
             ?OP_PUBKEYHASH          -> op_pubkeyhash;
             ?OP_PUBKEY              -> op_pubkey;
             ?OP_INVALIDOPCODE       -> op_invalidopcode;
             ?OP_RESERVED            -> op_reserved;
             ?OP_VER                 -> op_ver;
             ?OP_VERIF               -> op_verif;
             ?OP_VERNOTIF            -> op_vernotif;
             ?OP_RESERVED1           -> op_reserved1;
             ?OP_RESERVED2           -> op_reserved2;
             ?OP_NOP1                -> op_nop1;
             ?OP_NOP2                -> op_nop2;
             ?OP_NOP3                -> op_nop3;
             ?OP_NOP4                -> op_nop4;
             ?OP_NOP5                -> op_nop5;
             ?OP_NOP6                -> op_nop6;
             ?OP_NOP7                -> op_nop7;
             ?OP_NOP8                -> op_nop8;
             ?OP_NOP9                -> op_nop9;
             ?OP_NOP10               -> op_nop10
         end,
    {Op, Binary0}.

%% @doc Unpack an (not)if operation
unpack_op_if(Binary0) ->
    {Script,  Binary1} = unpack_until(Binary0, op_endif),
    NotElse = fun(op_else) -> false;
                 (_)       -> true
              end,
    {BranchA, [_|BranchB]} = lists:splitwith(NotElse, Script),
    {BranchA, BranchB, Binary1}.

%% @doc Unpack until a certain operation is hit,
%%      e.g unpack until op_else
unpack_until(Binary, Op) ->
    unpack_until(Binary, Op, []).
unpack_until(Binary0, Op, Acc) ->
    case unpack_op(Binary0) of
        {Op,  Binary1} -> {lists:reverse(Acc), Binary1};
        {OpX, Binary1} -> unpack_until(Binary1, Op, [OpX|Acc])
    end.


