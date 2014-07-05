%%
%% Defines
%%
-define(NETWORK_MAIN, 16#D9B4BEF9).
-define(NETWORK_TESTNET, 16#DAB5BFFA).
-define(NETWORK_TESTNET3, 16#0709110B).
-define(NETWORK_NAMECOIN, 16#FEB4BEF9).
-define(PAD(Bytes), <<0:Bytes/unit:8>>).

-define(OBJECT_TYPE_ERROR, 0).
-define(OBJECT_TYPE_MSG_TX, 1).
-define(OBJECT_TYPE_MSG_BLOCK, 2).
-define(OBJECT_TYPE_MSG_FILTERED_BLOCK, 3).

-define(REJECT_MALFORMED,       16#01).
-define(REJECT_INVALID,         16#10).
-define(REJECT_OBSOLETE,        16#11).
-define(REJECT_DUPLICATE,       16#12).
-define(REJECT_NONSTANDARD,     16#40).
-define(REJECT_DUST,            16#41).
-define(REJECT_INSUFFICIENTFEE, 16#42).
-define(REJECT_CHECKPOINT,      16#43).

-define(BLOOM_UPDATE_NONE, 0).
-define(BLOOM_UPDATE_ALL, 1).
-define(BLOOM_UPDATE_ONLY_P2PUBKEY_ONLY, 2).

-define(CORE_PUB_KEY, <<
                        16#04, % Full key
                        16#fc9702847840aaf195de8442ebecedf5b095cdbb9bc716bda9110971b28a49e0:32/unit:8, %% X
                        16#ead8564ff0db22209e0374782c093bb899692d524e9d6a6956e7c5ecbcd68284:32/unit:8  %% Y
                      >>).

-define(SERVICE_NODE_NETWORK, 1).

%%
%% Record definitions
%%

-record(message, {
          network  :: network(),
          command  :: command(),
          length   :: length(),
          checksum :: checksum(),
          payload  :: payload()
         }).  

-record(net_addr, {
          time     :: integer(),
          services :: services(),
          ip       :: ipaddr(),
          port     :: portnum()
         }).

-record(inv_vect, {
          type :: object_type(),
          hash :: hash()
         }).

-record(version, {
          version      :: integer(),
          services     :: services(),
          timestamp    :: timestamp(),
          addr_recv    :: #net_addr{},
          addr_from    :: #net_addr{},
          nounce       :: uinteger(), 
          user_agent   :: string(), 
          start_height :: integer(), 
          relay        :: boolean()
         }).  

-record(verack, {}).

-record(addr, {addr_list :: [#net_addr{}]}).

-record(inv, {inventory :: inventory()}).

-record(getdata, {inventory :: inventory()}).

-record(notfound, {inventory :: inventory()}).

-record(getblocks, {
         version   :: uinteger(),
         locator   :: locator(),
         hash_stop :: hash()
         }).

-record(getheaders, {
          version   :: uinteger(),
          locator   :: locator(),
          hash_stop :: hash()
         }).

-record(outpoint, {
          hash  :: hash(),
          index :: uinteger()
         }).

-record(tx_in, {
          previous_output :: #outpoint{},
          script          :: binary(),
          sequence        :: uinteger()
         }).

-record(tx_out, {
          value  :: integer(),
          script :: binary()
         }).

-record(tx, {
          version   :: uinteger(),
          tx_in     :: array(#tx_in{}),
          tx_out    :: array(#tx_out{}),
          lock_time :: uinteger()
         }).

-record(block, {
          version     :: uinteger(),
          prev_block  :: hash(),
          merkle_root :: hash(),
          timestamp   :: timestamp(),
          bits        :: uinteger(),
          nounce      :: uinteger(),
          txns        :: array(#tx{})
         }).

-record(headers, {headers :: array(#block{})}).

-record(getaddr, {}).

-record(mempool, {}).

-record(ping, {nounce :: integer()}).

-record(pong, {nounce :: integer()}).

-record(reject, {
          message :: binary(),
          ccode   :: byte(),
          reason  :: binary()
         }).

-record(filterload, {
          filter      :: binary(),
          n_hash_funs :: uinteger(),
          n_tweak     :: uinteger(),
          n_flags     :: filter_flags()
         }).

-record(filteradd, {data :: binary()}).

-record(filterclear, {}).

-record(merkleblock, {
          version     :: uinteger(),
          prev_block  :: hash(),
          merkle_root :: hash(),
          timestamp   :: timestamp(),
          bits        :: uinteger(),
          nounce      :: uinteger(),
          total_txns  :: pos_integer(),
          hashes      :: [hash()],
          flags       :: [0 | 1]
         }).

-record(alert, {
          version     :: integer(),
          relay_until :: timestamp(),
          expiration  :: timestamp(),
          id          :: integer(),
          cancel      :: integer(),
          set_cancel  :: set:set(integer()),
          min_ver     :: integer(),
          max_ver     :: integer(),
          set_sub_ver :: integer(),
          priority    :: integer(),
          comment     :: binary(),
          status_bar  :: binary(),
          reserved    :: binary()
         }).

-type network() :: main | testnet | testnet3 | namecoin. 
-type magic() :: ?NETWORK_MAIN | ?NETWORK_TESTNET |
                 ?NETWORK_TESTNET3 | ?NETWORK_NAMECOIN.

-type command_bin() :: <<_:96>>.
-type command_without_payload() :: 
          verack |
          getaddr |
          mempool |
          filterclear.

-type command_with_payload() ::
          version |
          addr |
          inv |
          getdata |
          notfound |
          getblocks |
          getheaders |
          tx |
          block |
          headers |
          ping |
          pong |
          filterload |
          filteradd |
          merkleblock |
          alert.

-type command() :: command_without_payload() | command_with_payload().

-type message_header_bin() :: <<_:192>>.
-type message_bin() :: <<_:192, _:_*8>>.
-type payload() ::
          #version{} |
          #addr{} |
          #inv{} |
          #getdata{} |
          #notfound{} |
          #getblocks{} |
          #getheaders{} |
          #tx{} |
          #block{} |
          #headers{} |
          #ping{} |
          #pong{} |
          #filterload{} |
          #filteradd{} |
          #merkleblock{} |
          #alert{} |
          undefined.

-type message_type() :: command_without_payload() | payload().

%% Various types
-type uinteger() :: non_neg_integer().
-type length() :: non_neg_integer().
-type index() :: non_neg_integer().
-type timestamp() :: erlang:timestamp().
-type array(T) :: array:array(T).
-type socket()  :: inet:socket().
-type ipaddr()  :: inet:ip_address().
-type portnum() :: inet:port_number().
-type address() :: {ipaddr(), portnum()}.
-type checksum() :: <<_:32>>.

%% Version types
-type service()  :: node_network.
-type services() :: [service()].

%% Inventory types
-type object_type() :: error | transaction | block | filtered_block.
-type hash()        :: binary().
-type inventory()   :: array(#inv_vect{}).
-type locator()     :: array(hash()).

%% Filter types
-type filter_flag()  :: none | all | p2pubkey_only.
-type filter_flags() :: [filter_flag()].

-type active_peers() :: [active_peer()].
-type active_peer()  ::
          {pid(), connecting, address(), timestamp()} |
          {pid(), connected, #version{}, timestamp()}.
