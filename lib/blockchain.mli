type block
(**Type of block in blockchain. Blocks hold a pool of transactions, its
   identifying hash, the hash of the block previous to it in the blockchain, the
   time stamp of when the block was made, and its nonce*)

type t
(** Type of blockchain, which represents a list of blocks, where each block
    holds a group of transactions*)

val get_nonce : block -> string
(** [get_nonce b] returns the nonce of block [b]*)

val get_time : block -> float
(** [get_time b] returns the time of block [b]*)

val get_prev_hash : block -> string
(** [get_prev_hash b] returns the prev_hash of block [b]*)

val get_hash : block -> string
(** [get_hash b] returns the hash of block [b]*)

val get_transaction_list : block -> Transaction.t list
(** [get_transaction_list b] returns the transaction list of block [b]*)

val create_block : Transaction.t list -> string -> string -> string -> block
(** [create_block t_list n p h] creates a block with transaction list [t_list],
    nonce [n], prev_hash [p], and hash [h]*)

val block_to_string : block -> string

val calculate_hash : block -> string
(**[calculate_hash block] calculates the hash of the block using the blocks
   internal information.*)

val verify_block : block -> bool
(**[verify_block b] checks the structure validity of block [b]*)

val get_block : t -> string -> block option
(** [get_block blockchain hash] is the block with [hash] in the [blockchain]*)

val mine_block : block -> int -> block
(**[mine_block blockchain transaction_list difficulty] mines a block into the
   [blockchain] with the [transaction_list] using the [difficulty] *)

val create_genesis_block : unit -> block
(** [create_genesis_block] creates a genesis block to start the blockchain*)

val create_blockchain : unit -> t
(**[create_blockchain] creates a blockchain starting with genesis block in
   transaction list*)

val verify_blockchain : t -> bool
(**[verify_blockchain t] verifies the valididyt of blockchain t *)

val get_last_block : t -> block
(**[get_last_block t] returns the last block of blockchain [t]*)

val add_block : t -> block -> t
(**[add_block t b] adds block [b] to blockchain [t]*)

val blockchain_length : t -> int
(**[blockchain_length t] returns the length of blockchain [t]*)

val init : unit -> t
(** [init ()] initializes the blockchain via a genesis block*)
