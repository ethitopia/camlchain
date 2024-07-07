open GroupProject
open OUnit2
open Transaction
open Account
open Blockchain

let db_path = "accounts.db"
let sql_script_path = "init_db.sql"
let () = Account.initialize_db db_path sql_script_path

let setup_db () =
  if Sys.file_exists db_path then Sys.remove db_path;
  initialize_db db_path sql_script_path

let teardown_db () =
  if Sys.file_exists db_path then
    Sys.remove db_path (* let example_blockchain = Blockchain.init () *)

let accounttests =
  [
    ( "test_create_account_with_private_key_name_balance" >:: fun _ ->
      let account =
        create_account_object_with_private_key "Alice" 1000 "public_key_1234"
          "private_key_5678"
      in
      assert_equal "Alice" (get_name account);
      assert_equal 1000 (get_balance account) );
    ( "test_create_account_with_private_key_keys" >:: fun _ ->
      let account =
        create_account_object_with_private_key "Bob" 500 "public_key_5678"
          "private_key_1234"
      in
      assert_equal "public_key_5678" (get_public_key account);
      assert_equal "private_key_1234" (get_private_key account) );
    ( "test_create_account_object_name" >:: fun _ ->
      let account = create_account_object "Alice" 1000 "1000" in
      assert_equal (get_name account) "Alice" );
    ( "test_create_account_object_balance" >:: fun _ ->
      let account = create_account_object "Alice" 1000 "1000" in
      assert_equal (get_balance account) 1000 );
    ( "test_create_account_object_public_key" >:: fun _ ->
      let account = create_account_object "Alice" 1000 "1000" in
      assert_bool "Public key should not be empty"
        (String.length (get_public_key account) > 0) );
    ( "test_create_account_object_private_key" >:: fun _ ->
      let account = create_account_object "Alice" 1000 "1000" in
      assert_bool "Private key should not be empty"
        (String.length (get_private_key account) > 0) );
    ( "test_encrypt" >:: fun _ ->
      let key = "1234" in
      let shift = 3 in
      let encrypted = encrypt key shift in
      assert_equal "4567" encrypted );
    ( "test_decrypt" >:: fun _ ->
      let key = "4567" in
      let shift = 3 in
      let decrypted = decrypt key shift in
      print_endline
        ("Decrypted " ^ key ^ " with shift " ^ string_of_int shift ^ ": "
       ^ decrypted);
      assert_equal "1234" decrypted );
    ( "test_encrypt_decrypt" >:: fun _ ->
      let key = "0987" in
      let shift = 5 in
      let encrypted = encrypt key shift in
      let decrypted = decrypt encrypted shift in
      print_endline ("Original key: " ^ key);
      print_endline ("Encrypted key: " ^ encrypted);
      print_endline ("Decrypted key: " ^ decrypted);
      assert_equal key decrypted );
    ( "test_encrypt_with_wraparound" >:: fun _ ->
      let key = "7890" in
      let encrypted = encrypt key 4 in
      assert_equal "1234" encrypted );
    ( "test_decrypt_with_wraparound" >:: fun _ ->
      let key = "1234" in
      let shift = 4 in
      let decrypted = decrypt key shift in
      print_endline
        ("Decrypted " ^ key ^ " with shift " ^ string_of_int shift ^ ": "
       ^ decrypted);
      assert_equal "7890" decrypted );
    ( "test_verify_account_by_public_key" >:: fun _ ->
      setup_db ();
      let account = create_account_object "Grace" 800 "800" in
      add_account db_path account;
      let pub_key = get_public_key account in
      assert_bool "Account should be verified by public key"
        (verify_account_by_public_key pub_key db_path);
      assert_bool "Non-existing public key should not be verified"
        (not (verify_account_by_public_key "invalid_key" db_path));
      teardown_db () );
    ( "test_create_account_name_balance" >:: fun _ ->
      let account1 = create_account_object "Alice" 1000 "1234" in
      let account2 =
        create_account_object_with_private_key "Bob" 500 "5678" "91011"
      in
      assert_equal "Alice" (get_name account1);
      assert_equal 1000 (get_balance account1);
      assert_equal "Bob" (get_name account2);
      assert_equal 500 (get_balance account2) );
    ( "test_create_account_keys" >:: fun _ ->
      let account1 = create_account_object "Alice" 1000 "1234" in
      let account2 =
        create_account_object_with_private_key "Bob" 500 "5678" "91011"
      in
      assert_equal "1234" (get_public_key account1);
      assert_equal (encrypt "1234" 4) (get_private_key account1);
      assert_equal "5678" (get_public_key account2);
      assert_equal "91011" (get_private_key account2) );
    ( "test_create_account_with_private_key_name_balance" >:: fun _ ->
      let account1 = create_account_object "Charlie" 2000 "1122" in
      let account2 =
        create_account_object_with_private_key "Dave" 1500 "3344" "5566"
      in
      assert_equal "Charlie" (get_name account1);
      assert_equal 2000 (get_balance account1);
      assert_equal "Dave" (get_name account2);
      assert_equal 1500 (get_balance account2) );
    ( "test_create_account_with_private_key_keys" >:: fun _ ->
      let account1 = create_account_object "Charlie" 2000 "1122" in
      let account2 =
        create_account_object_with_private_key "Dave" 1500 "3344" "5566"
      in
      assert_equal "1122" (get_public_key account1);
      assert_equal (encrypt "1122" 4) (get_private_key account1);
      assert_equal "3344" (get_public_key account2);
      assert_equal "5566" (get_private_key account2) );
  ]

let create_sample_transaction () =
  let sender = create_account_object "Alice" 1000 "1000" in
  let recipient = create_account_object "Bob" 500 "2000" in
  let amount = "100" in
  let signature = "1000" in
  create_transaction " " sender recipient amount signature

let transaction_tests =
  [
    ( "test_to_string" >:: fun _ ->
      let transaction = create_sample_transaction () in
      let expected = "AliceBob100" in
      assert_equal expected (to_string transaction) );
    ( "test_get_signature" >:: fun _ ->
      let transaction = create_sample_transaction () in
      let expected = "5444" in
      assert_equal expected (get_signature transaction) );
    ( "test_get_amount" >:: fun _ ->
      let transaction = create_sample_transaction () in
      let expected = "100" in
      assert_equal expected (get_amount transaction) );
    ( "test_get_sender" >:: fun _ ->
      let transaction = create_sample_transaction () in
      let sender = get_sender transaction in
      assert_equal "Alice" (get_name sender);
      assert_equal 900 (get_balance sender) );
    ( "test_get_recipient" >:: fun _ ->
      let transaction = create_sample_transaction () in
      let recipient = get_recipient transaction in
      assert_equal "Bob" (get_name recipient) );
    ( "test_verify_transaction" >:: fun _ ->
      let sender = create_account_object "seb" 10000 "1000" in
      let recipient = create_account_object "chris" 200000 "1000" in
      let priv_key = get_private_key sender in
      let amount = "100" in
      let transaction =
        create_transaction " " sender recipient amount priv_key
      in
      let is_valid = verify_transaction2 transaction in
      print_endline ("Transaction valid: " ^ string_of_bool is_valid);
      assert_bool "Transaction should be valid" is_valid );
    ( "let test_verify_transaction " >:: fun _ ->
      let sender = create_account_object "seb" 10000 "1000" in
      let recipient = create_account_object "chris" 200000 "1000" in
      let priv_key = get_private_key sender in
      let amount = "100" in
      let transaction =
        create_transaction " " sender recipient amount priv_key
      in
      let is_valid = verify_transaction2 transaction in
      print_endline ("Transaction valid: " ^ string_of_bool is_valid);
      assert_bool "Transaction should be valid" is_valid );
    ( "test_create_transaction_check_balances" >:: fun _ ->
      let sender = create_account_object "John" 5000 "1000" in
      let recipient = create_account_object "Doe" 3000 "2000" in
      let pub_key = get_public_key sender in
      let amount = "500" in
      let transaction =
        create_transaction " " sender recipient amount pub_key
      in
      assert_equal (get_amount transaction) amount );
    ( "test_invalid_transaction_same_sender_recipient" >:: fun _ ->
      let account = create_account_object "Alice" 1000 "1000" in
      let amount = "100" in
      assert_raises (Failure "ERROR: Invalid transaction") (fun () ->
          ignore
            (create_transaction " " account account amount
               (get_public_key account));
          print_endline "did not error") );
    ( "test_transaction_amount_greater_than_balance" >:: fun _ ->
      let sender = create_account_object "Alice" 100 "1000" in
      let recipient = create_account_object "Bob" 200 "2000" in
      let amount = "2000" in
      assert_raises (Failure "ERROR: Invalid transaction") (fun () ->
          create_transaction " " sender recipient amount (get_public_key sender))
    );
    (* Test case for creating a valid transaction and checking balances *)
    ( "test_create_transaction_check_balances" >:: fun _ ->
      let sender = create_account_object "John" 5000 "1000" in
      let recipient = create_account_object "Doe" 3000 "2000" in
      let amount = "500" in
      let transaction =
        create_transaction " " sender recipient amount (get_public_key sender)
      in
      assert_equal (get_amount transaction) amount );
    ( "test_create_multiple_transactions" >:: fun _ ->
      let sender1 = create_account_object "Eve" 6000 "1000" in
      let recipient1 = create_account_object "Sam" 4000 "2000" in
      let sender2 = create_account_object "Max" 5000 "3000" in
      let recipient2 = create_account_object "Ray" 3000 "4000" in
      let amount1 = "600" in
      let amount2 = "700" in
      let transaction1 =
        create_transaction " " sender1 recipient1 amount1
          (get_public_key sender1)
      in
      let transaction2 =
        create_transaction " " sender2 recipient2 amount2
          (get_public_key sender2)
      in
      assert_equal (get_amount transaction1) amount1;
      assert_equal (get_amount transaction2) amount2 );
    ( "test_invalid_transaction_non_existent_recipient" >:: fun _ ->
      let sender = create_account_object "Alice" 100 "1000" in
      let recipient = create_account_object "Bob" 200 "2000" in
      let amount = "200" in
      assert_raises (Failure "ERROR: Invalid transaction") (fun () ->
          create_transaction " " sender recipient amount (get_public_key sender))
    );
    ( "test_transaction_zero_amount" >:: fun _ ->
      let sender = create_account_object "Alice" 1000 "1000" in
      let recipient = create_account_object "Bob" 500 "2000" in
      let transaction = create_transaction " " sender recipient "0" "1000" in
      assert_bool "transaction of 0 should pass"
        (verify_transaction2 transaction) );
  ]

let create_sample_block () =
  let sender = create_account_object "Alice" 1000 "1000" in
  let recipient = create_account_object "Bob" 500 "2000" in
  let transaction = create_transaction " " sender recipient "100" "1000" in
  create_block [ transaction ] "1" "25" "35"

let blockchain_tests =
  [
    ( "test_get_nonce" >:: fun _ ->
      let block = create_sample_block () in
      let nonce = get_nonce block in
      print_endline "Expected nonce: 1";
      print_endline ("Actual nonce: " ^ nonce);
      assert_equal "1" nonce );
    ( "test_get_time" >:: fun _ ->
      let block = create_sample_block () in
      let time = get_time block in
      print_endline ("Expected timestamp: " ^ string_of_float (get_time block));
      print_endline ("Actual timestamp: " ^ string_of_float time);
      assert_equal (get_time block) time );
    ( "test_create_genesis_block" >:: fun _ ->
      let genesis_block = create_genesis_block () in
      assert_equal (get_prev_hash genesis_block) "0";
      assert_equal (List.length (get_transaction_list genesis_block)) 1;
      assert_equal (get_nonce genesis_block) "0" );
    ( "test_get_prev_hash" >:: fun _ ->
      let block = create_sample_block () in
      let prev_hash = get_prev_hash block in
      assert_equal "25" prev_hash );
    ( "test_get_hash" >:: fun _ ->
      let block = create_sample_block () in
      let hash = get_hash block in
      print_endline "Expected hash: 35";
      print_endline ("Actual hash: " ^ hash);
      assert_equal "35" hash );
    ( "test_create_block" >:: fun _ ->
      let sender = create_account_object "Alice" 1000 "1000" in
      let recipient = create_account_object "Bob" 500 "2000" in
      let priv_key = get_private_key sender in
      let transaction =
        create_transaction " " sender recipient "100" priv_key
      in
      let block = create_block [ transaction ] "0" "previous_hash" "hash" in
      assert_equal (List.length (get_transaction_list block)) 1;
      assert_equal (get_nonce block) "0";
      assert_equal (get_prev_hash block) "previous_hash" );
    ( "test_mine_block" >:: fun _ ->
      let sender = create_account_object "Alice" 1000 "1000" in
      let recipient = create_account_object "Bob" 500 "2000" in
      let priv_key = get_private_key sender in
      let transaction =
        create_transaction " " sender recipient "100" priv_key
      in
      let block = create_block [ transaction ] "0" "previous_hash" "hash" in
      let mined_block = mine_block block 2 in
      assert_bool "Mined block hash should start with 00"
        (String.sub (get_hash mined_block) 0 2 = "00") );
    ( "test_create_block_with_multiple_transactions" >:: fun _ ->
      let sender1 = create_account_object "Alice" 1000 "1000" in
      let recipient1 = create_account_object "Bob" 500 "2000" in
      let sender2 = create_account_object "Charlie" 2000 "3000" in
      let recipient2 = create_account_object "David" 1000 "4000" in
      let transaction1 =
        create_transaction " " sender1 recipient1 "100" (get_public_key sender1)
      in
      let transaction2 =
        create_transaction " " sender2 recipient2 "200" (get_public_key sender2)
      in
      let block =
        create_block [ transaction1; transaction2 ] "0" "previous_hash" "hash"
      in
      assert_equal (List.length (get_transaction_list block)) 2;
      assert_equal (get_nonce block) "0";
      assert_equal (get_prev_hash block) "previous_hash" );
    ( "test_add_block_to_chain" >:: fun _ ->
      let blockchain = create_blockchain () in
      let sender = create_account_object "Alice" 1000 "1000" in
      let recipient = create_account_object "Bob" 500 "2000" in
      let priv_key = get_private_key sender in
      let transaction =
        create_transaction " " sender recipient "100" priv_key
      in
      let last_block = get_last_block blockchain in
      let block = create_block [ transaction ] "0" (get_hash last_block) "" in
      let mined_block = mine_block block 2 in
      let new_blockchain = add_block blockchain mined_block in
      assert_equal (blockchain_length new_blockchain) 2 );
    ( "test_verify_blockchain" >:: fun _ ->
      let blockchain = create_blockchain () in
      let sender = create_account_object "Alice" 1000 "1000" in
      let recipient = create_account_object "Bob" 500 "2000" in
      let priv_key = get_private_key sender in
      let transaction =
        create_transaction " " sender recipient "100" priv_key
      in
      let block =
        create_block [ transaction ] "0"
          (get_hash (get_last_block blockchain))
          ""
      in
      let mined_block = mine_block block 2 in
      let new_blockchain = add_block blockchain mined_block in
      assert_bool "Blockchain should be valid"
        (verify_blockchain new_blockchain) );
    ( "test_add_multiple_blocks_to_chain" >:: fun _ ->
      let blockchain = create_blockchain () in
      let sender1 = create_account_object "Alice" 1000 "1000" in
      let recipient1 = create_account_object "Bob" 500 "2000" in
      let sender2 = create_account_object "Charlie" 2000 "3000" in
      let recipient2 = create_account_object "David" 1000 "4000" in
      let transaction1 =
        create_transaction " " sender1 recipient1 "100" (get_public_key sender1)
      in
      let transaction2 =
        create_transaction " " sender2 recipient2 "200" (get_public_key sender2)
      in
      let last_block = get_last_block blockchain in
      let block1 = create_block [ transaction1 ] "0" (get_hash last_block) "" in
      let mined_block1 = mine_block block1 2 in
      let new_blockchain1 = add_block blockchain mined_block1 in
      let block2 =
        create_block [ transaction2 ] "0"
          (get_hash (get_last_block new_blockchain1))
          ""
      in
      let mined_block2 = mine_block block2 2 in
      let new_blockchain2 = add_block new_blockchain1 mined_block2 in
      assert_equal (blockchain_length new_blockchain2) 3 );
    ( "test_verify_block" >:: fun _ ->
      let sender = create_account_object "Alice" 1000 "1000" in
      let recipient = create_account_object "Bob" 500 "2000" in
      let transaction =
        create_transaction " " sender recipient "100" (get_public_key sender)
      in
      let block = create_block [ transaction ] "0" "previous_hash" "hash" in
      let mined_block = mine_block block 2 in
      assert_bool "Block should be valid" (verify_block mined_block) );
    ( "test_get_block_by_hash" >:: fun _ ->
      let blockchain = create_blockchain () in
      let sender = create_account_object "Alice" 1000 "1000" in
      let recipient = create_account_object "Bob" 500 "2000" in
      let transaction =
        create_transaction " " sender recipient "100" (get_public_key sender)
      in
      let block = create_block [ transaction ] "0" "previous_hash" "hash" in
      let mined_block = mine_block block 2 in
      let new_blockchain = add_block blockchain mined_block in
      let retrieved_block = get_block new_blockchain (get_hash mined_block) in
      match retrieved_block with
      | Some blk -> assert_equal (get_hash blk) (get_hash mined_block)
      | None -> assert_failure "Block should be found by hash" );
    ( "test_get_transaction_list" >:: fun _ ->
      let block = create_sample_block () in
      let transaction_list = get_transaction_list block in
      print_endline "Expected transaction list length: 1";
      print_endline
        ("Actual transaction list length: "
        ^ string_of_int (List.length transaction_list));
      assert_equal 1 (List.length transaction_list);
      let transaction = List.hd transaction_list in
      assert_equal "Alice" (get_name (get_sender transaction));
      assert_equal "Bob" (get_name (get_recipient transaction)) );
    ( "test_block_to_string" >:: fun _ ->
      let block = create_sample_block () in
      let expected =
        Printf.sprintf
          "Block { transaction_list = [ AliceBob100 ]; nonce = %s; timestamp = \
           %f; prev_hash = %s; hash = %s }"
          (get_nonce block) (get_time block) (get_prev_hash block)
          (get_hash block)
      in
      let actual = block_to_string block in
      print_endline ("Expected block to_string: " ^ expected);
      print_endline ("Actual block to_string: " ^ actual);
      assert_equal expected actual );
    ( "test_get_last_block" >:: fun _ ->
      let blockchain = create_blockchain () in
      let last_block = get_last_block blockchain in
      assert_equal (get_nonce last_block) "0" );
    ( "test_verify_empty_blockchain" >:: fun _ ->
      let empty_blockchain = create_blockchain () in
      assert_bool "Empty blockchain should be valid"
        (verify_blockchain empty_blockchain) );
    ( "test_add_invalid_block_to_chain" >:: fun _ ->
      let blockchain = create_blockchain () in
      let invalid_block = create_block [] "0" "invalid_hash" "invalid_hash" in
      assert_raises (Failure "Blockchain and Block not compatible") (fun () ->
          add_block blockchain invalid_block) );
    ( "test_blockchain_length_initial" >:: fun _ ->
      let blockchain = create_blockchain () in
      let length = blockchain_length blockchain in
      print_endline ("Initial blockchain length: " ^ string_of_int length);
      assert_equal 1 length );
    ( "test_add_block_invalid_block" >:: fun _ ->
      let blockchain = create_blockchain () in
      let invalid_block = create_block [] "1" "0" "invalid_hash" in
      assert_raises (Failure "Blockchain and Block not compatible") (fun () ->
          add_block blockchain invalid_block) );
  ]

let test_suite =
  "Set test suite" >::: transaction_tests @ blockchain_tests @ accounttests

let _ = run_test_tt_main test_suite
