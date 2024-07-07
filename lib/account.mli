type account
(** Represents the accounts, and includes information on account name, id,
    balance*)

val decrypt : string -> int -> string
(** [decrypt s i] decrypts encrypted string of digits [s] with shift of [i]*)

val encrypt : string -> int -> string
(** [encrypt s i] encrypts string of digits [s] with shift [i]*)

val create_account_object : string -> int -> string -> account
(** [create_account  name balance publickey] creates an account object. *)

val create_account_object_with_private_key :
  string -> int -> string -> string -> account
(** [create_account name balance publickey privatekey] creates an account object
    with inputted private key*)

val get_name : account -> string
(** [get_name account] gets the name associated with the account*)

val get_public_key : account -> string
(**[get_public_id account] is the account’s public id*)

val get_balance : account -> int
(** [get_balance account] gets accout’s balance*)

val get_private_key : account -> string
(** [get_private_key account] is account's private key*)

val db_file_exists : Sqlite3.header -> bool
(**[db_file_exists database_path] is true if a database file exists at
   [database_path], else false*)

val initialize_db : Sqlite3.header -> Sqlite3.header -> unit
(**[initialize_db database_path sql_path] initializes the database at
   [database_path] given an sql file at [sql_path]. Does nothing if database is
   initialized at [database_path]*)

val load_accounts : Sqlite3.header -> account list
(**[load_accounts database_path] is the list of all the accounts in the account
   database located at [database_path] *)

val to_string_gui : account -> string
(** [to_string_gui account] is a string representation of account to be used to
    view accounts in the gui*)

val to_string_gui_list : account list -> string list
(** [to_string_gui_list accountlist] maps to_string_gui to each account in the
    accountlist*)

val verify_account_by_name : string -> Sqlite3.header -> bool
(** [verify_account id database_path] verifies an account with [id] exists in
    the database located at [database_path]*)

val add_account : Sqlite3.header -> account -> unit
(** [add_account database_path name balance] adds an account with a [name] and
    [balance] into the database located at [database_path] and returns the id of
    that new account. Returns [None] if account could not be added *)

val get_account_by_name : Sqlite3.header -> string -> account option
(** [get_account_by_name db s] returns the account from database [db] with
    string [s]*)

val is_initialized : unit -> bool
(** [is_initialized] checks to see if there’s a database, and makes one if there
    isn’t.*)

val verify_account_by_public_key : string -> Sqlite3.header -> bool
(** [verify_account_by_public_key s db] verifies account with public key [s]
    within database [db] *)

val update_balance : Sqlite3.header -> string -> int -> bool -> unit
(** [update_balance db name amount p] updates balance of account [name] within
    database [db] by amount [amount]. If p is true, amount is added, if false,
    amount is subtracted. *)
