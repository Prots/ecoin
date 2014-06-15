%% Message header
-record(message_header, {network  :: network(),
                         command  :: command(), 
                         length   :: non_neg_integer(), 
                         checksum :: binary()}).  


%% net_addr
-record(net_addr, {time          :: undefined | integer(),
                   services = [] :: services(),
                   ip            :: ipaddr(),
                   port          :: portnum}).

%% inv_vect
-record(inv_vect, {type :: object_type(),
                   hash :: hash()}).

%% version
-record(version, {proto_ver    :: integer(),
                  services     :: list(service()),
                  timestamp    :: erlang:timestamp(),
                  receiving    :: #net_addr{},
                  sending      :: #net_addr{}, 
                  nounce       :: integer(), 
                  user_agent   :: string(), 
                  start_height :: integer(), 
                  relay        :: boolean()}).  

%% verack
-record(verack, {}).

%% addr
-record(addr, {addresses :: array:array(#net_addr{})}).

%% inv
-record(inv, {inventory :: inventory()}).

%% getdata
-record(getdata, {data :: inventory()}).

%% notfound
-record(notfound, {data :: inventory()}).

%% getblocks
-record(getblocks, {proto_ver :: protocol_version(),
                    locator   :: locator(),
                    hash_stop :: hash()}).

%% getheaders
-record(getheaders, {proto_ver :: protocol_version,
                     locator   :: locator(),
                     hash_stop :: hash()}).

%% outpoint
-record(outpoint, {hash  :: hash(),
                   index :: integer()}).

%% tx_in
-record(tx_in, {prev_output :: #outpoint{},
                script      :: script(),
                sequence    :: integer()}).

%% tx_out
-record(tx_out, {value      :: integer(),
                 script     :: script()}).

%% tx
-record(tx, {proto_ver :: protocol_version(),
             in        :: array:array(#tx_in{}),
             out       :: array:array(#tx_out{}),
             lock_time :: integer()}).

%% block
-record(block, {proto_ver   :: protocol_version(),
                prev_block  :: hash(),
                merkle_root :: hash(),
                timestamp   :: timestamp(),
                bits        :: integer(),
                nounce      :: integer(),
                txns        :: array:array(#tx{})}).

%% headers
-record(headers, {headers :: array:array(#block{})}).

%% getaddr
-record(getaddr, {}).

%% mempool
-record(mempool, {}).

%% ping
-record(ping, {nounce :: integer()}).

%% pong
-record(pong, {nounce :: integer()}).

%% filterload
-record(filterload, {filter       :: binary(),
                     hash_funs    :: non_neg_integer(),
                     tweak        :: non_neg_integer(),
                     filter_flags :: list(filter_flag())}).

%% filteradd
-record(filteradd, {data :: binary()}).

% filterclear
-record(filterclear, {}).

%% merkleblock
-record(merkleblock, {proto_ver   :: protocol_version(),
                      prev_block  :: hash(),
                      merkle_root :: hash(),
                      timestamp   :: timestamp(),
                      difficulty  :: non_neg_integer(),
                      nounce      :: non_neg_integer(),
                      total_tx    :: pos_integer(),
                      hashes      :: array:array(hash()),
                      flags       :: binary()}).
                      
%% alert
-record(alert, {data}).

%% Various types
-type protocol_version() :: integer().
-type timestamp()        :: erlang:timestamp().


%% Network types
-type message() :: #version{} | #verack{}.
-type network() :: main | testnet | testnet3 | namecoin. 

%% Message types
-type command() :: atom().
-type socket()  :: inet:socket().
-type ipaddr()  :: inet:ip_address().
-type portnum() :: inet:port_number().
-type address() :: {ipaddr(), portnum()}.

%% Version types
-type service()          :: node_network.
-type services()         :: list(service()).


%% Inventory types
-type object_type() :: error | transaction | block.
-type hash()        :: binary().
-type inventory()   :: array:array(#inv_vect{}).
-type locator()     :: array:array(hash()).

%% Filter types
-type filter_flag() :: none | all | p2pubkey_only.

%% Transaction types
-type script() :: undefined.
