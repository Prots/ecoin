%% General
-define(MAX_ELEMENT_SIZE, 520).
-define(MAX_OP_RETURN_RELAY, 40).

%% Constants
-define(OP_TRUE, 81).
-define(OP_FALSE, 0).
-define(OP_1NEGATE, 79).
-define(OP_0, 0).
-define(OP_1, 81).
-define(OP_2, 82).
-define(OP_3, 83).
-define(OP_4, 84).
-define(OP_5, 85).
-define(OP_6, 86).
-define(OP_7, 87).
-define(OP_8, 88).
-define(OP_9, 89).
-define(OP_10, 90).
-define(OP_11, 91).
-define(OP_12, 92).
-define(OP_13, 93).
-define(OP_14, 94).
-define(OP_15, 95).
-define(OP_16, 96).
-define(OP_N(Byte), Byte >= ?OP_1, Byte =< ?OP_16).
-define(OP_PUSHDATA1, 76).
-define(OP_PUSHDATA2, 77).
-define(OP_PUSHDATA4, 78).
-define(OP_PUSHDATA(Byte), Byte > 0, Byte < ?OP_PUSHDATA1).

%% Flow
-define(OP_NOP, 97).
-define(OP_IF, 99).
-define(OP_NOTIF, 100).
-define(OP_ELSE, 103).
-define(OP_ENDIF, 104).
-define(OP_VERIFY, 105).
-define(OP_RETURN, 106).

%% Stack
-define(OP_TOALTSTACK, 107).
-define(OP_FROMALTSTACK, 108).
-define(OP_IFDUP, 115).
-define(OP_DEPTH, 116).
-define(OP_DROP, 117).
-define(OP_2DROP, 109).
-define(OP_DUP, 118).
-define(OP_2DUP, 110).
-define(OP_3DUP, 111).
-define(OP_NIP, 119).
-define(OP_OVER, 120).
-define(OP_2OVER, 112).
-define(OP_PICK, 121).
-define(OP_ROLL, 122).
-define(OP_ROT, 123).
-define(OP_2ROT, 113).
-define(OP_SWAP, 124).
-define(OP_2SWAP, 114).
-define(OP_TUCK, 125).

%% Splice
-define(OP_CAT, 126).
-define(OP_SUBSTR, 127).
-define(OP_LEFT, 128).
-define(OP_RIGHT, 129).
-define(OP_SIZE, 130).

%% Bitwise logic
-define(OP_INVERT, 131).
-define(OP_AND, 132).
-define(OP_OR, 133).
-define(OP_XOR, 134).
-define(OP_EQUAL, 135).
-define(OP_EQUALVERIFY, 136).

%% Artithmetic
-define(OP_1ADD, 139).
-define(OP_1SUB, 140).
-define(OP_2MUL, 141).
-define(OP_2DIV, 142).
-define(OP_NEGATE, 143).
-define(OP_ABS, 144).
-define(OP_NOT, 145).
-define(OP_0NOTEQUAL, 146).
-define(OP_ADD, 147).
-define(OP_SUB, 148).
-define(OP_MUL, 149).
-define(OP_DIV, 150).
-define(OP_MOD, 151).
-define(OP_LSHIFT, 152).
-define(OP_RSHIFT, 153).
-define(OP_BOOLAND, 154).
-define(OP_BOOLOR, 155).
-define(OP_NUMEQUAL, 156).
-define(OP_NUMEQUALVERIFY, 157).
-define(OP_NUMNOTEQUAL, 158).
-define(OP_LESSTHAN, 159).
-define(OP_GREATERTHAN, 160).
-define(OP_LESSTHANOREQUAL, 161).
-define(OP_GREATERTHANOREQUAL, 162).
-define(OP_MIN, 163).
-define(OP_MAX, 164).
-define(OP_WITHIN, 165).

%% Crypto
-define(OP_RIPEMD160, 166).
-define(OP_SHA1, 167).
-define(OP_SHA256, 168).
-define(OP_HASH160, 169).
-define(OP_HASH256, 170).
-define(OP_CODESEPARATOR, 171).
-define(OP_CHECKSIG, 172).
-define(OP_CHECKSIGVERIFY, 173).
-define(OP_CHECKMULTISIG, 174).
-define(OP_CHECKMULTISIGVERIFY, 175).

%% Pseudo-words
-define(OP_PUBKEYHASH, 253).
-define(OP_PUBKEY, 254).
-define(OP_INVALIDOPCODE, 255).

%% Reserved words
-define(OP_RESERVED, 80).
-define(OP_VER, 98).
-define(OP_VERIF, 101).
-define(OP_VERNOTIF, 102).
-define(OP_RESERVED1, 137).
-define(OP_RESERVED2, 138).
-define(OP_NOP1, 176).
-define(OP_NOP2, 177).
-define(OP_NOP3, 178).
-define(OP_NOP4, 179).
-define(OP_NOP5, 180).
-define(OP_NOP6, 181).
-define(OP_NOP7, 182).
-define(OP_NOP8, 183).
-define(OP_NOP9, 184).
-define(OP_NOP10, 185).

%% Hash types
-define(SIGHASH_ALL, 16#00000001).
-define(SIGHASH_NONE, 16#00000002).
-define(SIGHASH_SINGLE, 16#00000003).
-define(SIGHASH_ANYONECANPAY, 16#00000080).

-type constants_operation() ::
          boolean() | integer() | binary().

-type flow_control_operation() ::
          'if' | nif | else | endif |
          nop | verify | return.

-type stack_operation() ::
          depth | toaltstack | fromaltstack |
          nip | pick | roll | tuck | ifdup |
          {dup,  1..3} |
          {drop, 1..2} |
          {over, 1..2} |
          {rot,  1..2} |
          {swap, 1..2}.

-type splice_operation() ::
          cat | substr | left | right | size.

-type bitwise_operation() ::
          invert | 'and' | 'or' | 'xor' |
          equal | equalverify.

-type arithmetic_operation() ::
          add1 | sub1 |
          add | sub |
          mul2 | div2 | lshift | rshift |
          mul | 'div' | mod |
          negate | abs | notequal0 |
          'not' | booland | boolor |
          numequal | numequalverify | numnotequal |
          lessthan | greaterthan |
          lessthanorequal | greaterthanorequal |
          min | max | within.

-type crypto_operation() ::
          codeseparator |
          ripemd160 | sha1 | sha256 | hash160 | hash256 |
          checksig | checksigverify |
          checkmultisig | checkmultisigverify.

-type script_operation() ::
          constants_operation() |
          flow_control_operation() |
          stack_operation() |
          splice_operation () |
          bitwise_operation() |
          arithmetic_operation() |
          crypto_operation().

-type script_enum() :: pk_script | sig_script.
-type script_raw() :: binary().
-type script() :: [script_operation].
