open Transaction
open Account

type block = {
  transaction_list : t list;
  nonce : string;
  timestamp : float;
  prev_hash : string;
  hash : string;
}

type t = block list

let get_nonce b = b.nonce
let get_time b = b.timestamp
let get_prev_hash b = b.prev_hash
let get_hash b = b.hash

(** [get_transaction_list b] returns transaction_list of block [b]*)
let get_transaction_list block = block.transaction_list

let create_block t_list nonce prev_h h =
  {
    transaction_list = t_list;
    nonce;
    timestamp = Unix.time ();
    prev_hash = prev_h;
    hash = h;
  }

(** [block_to_string b] returns a string of block [b]*)
let block_to_string block =
  let transactions_str =
    block.transaction_list
    |> List.map Transaction.to_string
    |> String.concat ", "
  in
  Printf.sprintf
    "Block { transaction_list = [ %s ]; nonce = %s; timestamp = %f; prev_hash \
     = %s; hash = %s }"
    transactions_str block.nonce block.timestamp block.prev_hash block.hash

(** [calculate hash b] calculates hash for block [b].*)
let calculate_hash block =
  let transaction_data = block_to_string { block with hash = "" } in
  let block_hash = Cryptokit.Hash.sha256 () in
  Cryptokit.transform_string (Cryptokit.Hexa.encode ())
    (Cryptokit.hash_string block_hash transaction_data)

let verify_block block =
  (* Check if the hash of the block is correctly calculated *)
  let hash_valid = block.hash = calculate_hash { block with hash = "" } in
  let transactions_valid =
    List.for_all verify_transaction2 block.transaction_list
  in
  hash_valid && transactions_valid

(** [get_block blockchain hash] gets the block from the blockchain that matches
    the given hash, in the form of an option *)
let get_block blockchain hash =
  List.find_opt (fun block -> block.hash = hash) blockchain

(** [mine_block b d] mines the block [b] with difficult [d], difficulty is the
    nonce field of 0's. *)
let mine_block block difficulty =
  let rec find_nonce nonce =
    let candidate_block = { block with nonce; hash = "" } in
    let hash = calculate_hash candidate_block in
    if String.sub hash 0 difficulty = String.make difficulty '0' then
      { candidate_block with hash }
    else find_nonce (string_of_int (int_of_string nonce + 1))
  in
  find_nonce "0"

(* Initialize the blockchain with the genesis block *)

(** [create_genesis_block] creates a genesis block for the beginning of each
    blockchain. *)
let create_genesis_block () =
  let sender = create_account_object "_" 0 "" in
  let recipient = create_account_object "_" 0 "" in
  let genesis_transaction =
    create_transaction " " sender recipient "0" "whatsup"
  in
  let genesis_transactions = [ genesis_transaction ] in
  let genesis_block =
    {
      transaction_list = genesis_transactions;
      nonce = "0";
      timestamp = Unix.time ();
      prev_hash = "0";
      hash = "";
    }
  in
  let genesis_hash = calculate_hash genesis_block in
  { genesis_block with hash = genesis_hash }

(** [create_blockchain] creates a blockchain with a genesis block as its first
    element. *)
let create_blockchain () = [ create_genesis_block () ]

let rec verify_blockchain = function
  | [] | [ _ ] -> true (* A blockchain with 0 or 1 block is valid *)
  | prev_block :: (block :: _ as rest) ->
      block.prev_hash = prev_block.hash
      && block.hash = calculate_hash { block with hash = "" }
      && List.for_all verify_transaction2 block.transaction_list
      && verify_blockchain rest

let get_last_block blockchain =
  match List.rev blockchain with
  | [] -> failwith "Blockchain is empty"
  | last_block :: _ -> last_block

let add_block blockchain block =
  if verify_blockchain blockchain && verify_block block then
    blockchain @ [ block ]
  else failwith "Blockchain and Block not compatible"

let blockchain_length blockchain = List.length blockchain
let init = create_blockchain
