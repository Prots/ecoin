
%%
%% Record defitions
%%

-record(message, {
          magic    :: network(),
          length   :: length(),
          checksum :: binary(),
          payload  :: payload()
         }).  

-record(net_addr, {
          time          :: undefined | integer(),
          services = [] :: [service()],
          ip            :: ipaddr(),
          port          :: portnum()
         }).

-record(inv_vect, {
          type :: object_type(),
          hash :: hash()
         }).

-record(version, {
          version      :: integer(),
          services     :: [service()],
          timestamp    :: timestamp(),
          addr_recv    :: #net_addr{},
          addr_from    :: #net_addr{},
          nounce       :: uinteger(), 
          user_agent   :: string(), 
          start_height :: integer(), 
          relay        :: boolean()
         }).  

-record(verack, {}).

-record(addr, {addr_list :: array(#net_addr{})}).

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

-record(filterload, {
          filter       :: binary(),
          hash_funs    :: uinteger(),
          tweak        :: uinteger(),
          filter_flags :: [filter_flag()]
         }).

-record(filteradd, {data :: binary()}).

-record(filterclear, {}).

-record(merkleblock, {
          version     :: integer(),
          prev_block  :: hash(),
          merkle_root :: hash(),
          timestamp   :: timestamp(),
          difficulty  :: uinteger(),
          nounce      :: uinteger(),
          total_tx    :: pos_integer(),
          hashes      :: array(hash()),
          flags       :: binary()
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

-type message() :: #message{}.
-type payload() ::
          #version{} |
          #verack{} |
          #addr{} |
          #inv{} |
          #getdata{} |
          #notfound{} |
          #getblocks{} |
          #getheaders{} |
          #tx{} |
          #block{} |
          #headers{} |
          #getaddr{} |
          #mempool{} |
          #ping{} |
          #pong{} |
          #filterload{} |
          #filteradd{} |
          #filterclear{} |
          #merkleblock{} |
          #alert{}.

-type net_addr() :: #net_addr{}.
-type inv_vect() :: #inv_vect{}.
-type tx_in() :: #tx_in{}.
-type tx_out() :: #tx_out{}.

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

%% Version types
-type service() :: node_network.

%% Inventory types
-type object_type() :: error | transaction | block | filtered_block.
-type hash()        :: binary().
-type inventory()   :: array(inv_vect()).
-type locator()     :: array(hash()).

%% Filter types
-type filter_flag() :: none | all | p2pubkey_only.
