open Account
open Sqlite3

let initialized = ref false

type t = {
  sender : account;
  recipient : account;
  amount : string;
  signature : string;
}

(** [to_string t] returns transaction [t] as a string*)
let to_string transaction =
  get_name transaction.sender
  ^ get_name transaction.recipient
  ^ transaction.amount

(** [to_string t] returns transaction [t] as a string*)
let to_string_gui transaction =
  Printf.sprintf "Sender: %s , Recipient : %s , Amount : %s"
    (get_name transaction.sender)
    (get_name transaction.recipient)
    transaction.amount

let get_signature transaction = transaction.signature
let get_amount transaction = transaction.amount
let get_sender transaction = transaction.sender
let get_recipient transaction = transaction.recipient

(** [verify_transaction t] verifies the validity of transaction [t]*)
let verify_transaction db_file transaction =
  if db_file = "" || db_file = " " then
    transaction.sender != transaction.recipient
    && int_of_string transaction.amount <= get_balance transaction.sender
  else (
    ();
    if
      transaction.sender != transaction.recipient
      && int_of_string transaction.amount <= get_balance transaction.sender
      && verify_account_by_name (get_name transaction.sender) db_file
      && verify_account_by_name (get_name transaction.recipient) db_file
    then
      get_public_key (get_sender transaction)
      = decrypt (get_signature transaction) 4
    else false)

let verify_transaction2 transaction =
  transaction.sender != transaction.recipient
  && int_of_string transaction.amount <= get_balance transaction.sender

let create_transaction db_file sender recipient amount public_key =
  let amount_int = int_of_string amount in
  let new_transaction =
    { sender; recipient; amount; signature = encrypt public_key 4 }
  in
  if verify_transaction db_file new_transaction then
    let updated_sender =
      create_account_object_with_private_key (get_name sender)
        (get_balance sender - amount_int)
        (get_public_key sender) (get_private_key sender)
    in
    let updated_recipient =
      create_account_object_with_private_key (get_name recipient)
        (get_balance sender - amount_int)
        (get_public_key sender) (get_private_key sender)
    in
    let updated_transaction =
      {
        new_transaction with
        sender = updated_sender;
        recipient = updated_recipient;
      }
    in
    updated_transaction
  else failwith "ERROR: Invalid transaction"

let create_transactions_no_checks sender recipient amount signature =
  { sender; recipient; amount; signature }

let read_file file_path =
  let ic = open_in file_path in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in_noerr ic;
      List.rev acc
  in
  String.concat "\n" (read_lines [])

let initialize_transaction_db db_path sql_script_path =
  if not (Account.db_file_exists db_path) then begin
    let db = Sqlite3.db_open db_path in
    let sql_script = read_file sql_script_path in
    match
      Sqlite3.exec db sql_script
      (*~cb:(fun _ -> ()) this is broken, not sure what this is supposed to be*)
    with
    | Rc.OK ->
        print_endline "Database initialized successfully.";
        initialized := true
    | Rc.ERROR (*msg not sure what msg is used for or where its defined?*)
    | Rc.MISUSE (*msg*) ->
        Printf.eprintf "Failed to initialize database:"
        (* %s\n*)
        (*msg*)
    | _ ->
        print_endline "Unexpected error during database initialization.";
        Sqlite3.db_close db |> ignore
  end
  else
    (* I believe we were missing a flag here that will mark the database as
       initialized, which im adding now*)
    initialized := true;
  print_endline "Database already exists and will not be re-initialized."

let add_transaction_to_db db_file transaction =
  if not !initialized then failwith "Database not initialized";
  let sender_name = get_name transaction.sender in
  let recipient_name = get_name transaction.recipient in

  let amount = transaction.amount in
  let signature = transaction.signature in
  let db = Sqlite3.db_open db_file in
  let sql =
    "INSERT INTO pending_transactions (sender_id, recipient_id, \
     sender_private_key, amount) VALUES (?, ?, ?,?)"
  in
  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT sender_name));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT recipient_name));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT signature));
  ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.INT (Int64.of_string amount)));
  match Sqlite3.step stmt with
  | Rc.DONE ->
      Printf.printf "transaction created";
      Sqlite3.finalize stmt |> ignore;
      Sqlite3.db_close db |> ignore;
      ()
  | _ ->
      Printf.printf "Transaction not created";
      Sqlite3.finalize stmt |> ignore;
      Sqlite3.db_close db |> ignore;
      ()

let load_transactions db_file db_account_file : t list =
  if not !initialized then failwith "Database not initialized";
  let db = Sqlite3.db_open db_file in
  let pending_transactions = ref [] in
  let extract_row stmt =
    match Sqlite3.step stmt with
    | Rc.ROW ->
        pending_transactions :=
          create_transactions_no_checks
            (Option.get
               (get_account_by_name db_account_file
                  (Sqlite3.column_text stmt 0)))
            (Option.get
               (get_account_by_name db_account_file
                  (Sqlite3.column_text stmt 1)))
            (Sqlite3.column_text stmt 3)
            (string_of_int (Sqlite3.column_int stmt 2))
          :: !pending_transactions;
        true
    | _ -> false
  in
  let stmt =
    Sqlite3.prepare db
      "SELECT sender_id, recipient_id, sender_private_key, amount FROM \
       pending_transactions"
  in
  while extract_row stmt do
    ()
  done;
  Sqlite3.finalize stmt |> ignore;
  Sqlite3.db_close db |> ignore;
  !pending_transactions

let clear_transaction_db db_path sql_path =
  if Sys.file_exists db_path then (
    Sys.remove db_path;
    initialize_transaction_db db_path sql_path)
