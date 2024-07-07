open Sqlite3

let initialized = ref false

type account = {
  name : string;
  balance : int;
  public_key : string;
  private_key : string;
}

(* Utility function to convert a character to an integer *)
let char_to_int c = int_of_char c - int_of_char '0'

(* Utility function to convert an integer to a character *)
let int_to_char i = char_of_int (i + int_of_char '0')

(* Function to encrypt a string of digits using a key *)
let encrypt key shift =
  let length = String.length key in
  let result = Bytes.create length in
  for i = 0 to length - 1 do
    let original_digit = char_to_int key.[i] in
    let encrypted_digit = (original_digit + shift) mod 10 in
    Bytes.set result i (int_to_char encrypted_digit)
  done;
  Bytes.to_string result

(* Function to decrypt an encrypted string of digits using a key *)
let decrypt key shift =
  let length = String.length key in
  let result = Bytes.create length in
  for i = 0 to length - 1 do
    let encrypted_digit = char_to_int key.[i] in
    let original_digit = (encrypted_digit - shift + 10) mod 10 in
    Bytes.set result i (int_to_char original_digit)
  done;
  Bytes.to_string result

(*let () = Random.self_init () let random_digit () = Random.int 10

  let rec gen_priv length = if length <= 0 then "" else let digit =
  string_of_int (random_digit ()) in digit ^ gen_priv (length - 1)*)

let create_account_object name balance public_key =
  let priv = encrypt public_key 4 in
  { name; balance; public_key; private_key = priv }

let create_account_object_with_private_key name balance public_key private_key =
  { name; balance; public_key; private_key }

let get_name account = account.name
let get_public_key account = account.public_key
let get_balance account = account.balance
let get_private_key account = account.private_key
let db_file_exists db_path = Sys.file_exists db_path

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

let initialize_db db_path sql_script_path =
  if not (db_file_exists db_path) then begin
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

let load_accounts db_file =
  if not !initialized then failwith "Database not initialized";
  let db = Sqlite3.db_open db_file in
  let accounts = ref [] in
  let extract_row stmt =
    match Sqlite3.step stmt with
    | Rc.ROW ->
        accounts :=
          create_account_object_with_private_key
            (Sqlite3.column_text stmt 0)
            (Sqlite3.column_int stmt 1)
            (Sqlite3.column_text stmt 2)
            (Sqlite3.column_text stmt 3)
          :: !accounts;
        true
    | _ -> false
  in
  let stmt =
    Sqlite3.prepare db
      "SELECT name, balance, public_key, private_key FROM accounts"
  in
  while extract_row stmt do
    ()
  done;
  Sqlite3.finalize stmt |> ignore;
  Sqlite3.db_close db |> ignore;
  !accounts

let to_string_gui account =
  Printf.sprintf "Name: %s , Balance: %i" account.name account.balance

let to_string_gui_list accountlist = List.map to_string_gui accountlist

let verify_account_by_name name db_file =
  let accounts = load_accounts db_file in
  List.exists (fun acc -> acc.name = name) accounts

let add_account db_file account =
  if not !initialized then failwith "Database not initialized";
  let db = Sqlite3.db_open db_file in
  let sql =
    "INSERT INTO accounts (name, balance, public_key, private_key) VALUES (?, \
     ?, ?, ?)"
  in
  let name = account.name in
  let balance = account.balance in
  let public_key = account.public_key in
  let private_key = account.private_key in

  let stmt = Sqlite3.prepare db sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT name));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.INT (Int64.of_int balance)));
  ignore (Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT public_key));
  ignore (Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT private_key));
  match Sqlite3.step stmt with
  | Rc.DONE ->
      Printf.printf "Account '%s' added successfully\n" name;
      Sqlite3.finalize stmt |> ignore;
      Sqlite3.db_close db |> ignore
  | _ ->
      Printf.printf "Failed to add account '%s'.\n" name;
      Sqlite3.finalize stmt |> ignore;
      Sqlite3.db_close db |> ignore

let is_initialized () = !initialized

let update_balance db_file name amount plus =
  if not !initialized then failwith "Database not initialized";
  let db = Sqlite3.db_open db_file in
  let sql = ref "" in
  if plus then sql := "UPDATE accounts SET balance = balance + ? WHERE name = ?"
  else sql := "UPDATE accounts SET balance = balance - ? WHERE name = ?";
  let stmt = Sqlite3.prepare db !sql in
  ignore (Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int amount)));
  ignore (Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT name));
  match Sqlite3.step stmt with
  | Rc.DONE ->
      Sqlite3.finalize stmt |> ignore;
      Sqlite3.db_close db |> ignore
  | _ ->
      Sqlite3.finalize stmt |> ignore;
      Sqlite3.db_close db |> ignore

let get_account_by_name db_file name =
  let account_list = load_accounts db_file in
  List.find_opt (fun x -> x.name = name) account_list

let verify_account_by_public_key public_key db_file =
  let accounts = load_accounts db_file in
  List.exists (fun acc -> acc.public_key = public_key) accounts
