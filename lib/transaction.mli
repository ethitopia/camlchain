type t
(**Represents a transaction, which includes information on whose sending the
   transaction, whose receiving the transaction, the amount of crypto involved
   in the transaction, and the key of the senders account, which remains private*)

val to_string : t -> string
(** [to_string t] concatenates fields of transaction [t] to string *)

val get_signature : t -> string
(** [get_signature t] returns the signature of transaction [t]*)

val get_amount : t -> string
(** [get_amount t] returns the amount of transaction [t]*)

val get_sender : t -> Account.account
(** [get_sender t] returns the sender of transaction [t]*)

val get_recipient : t -> Account.account
(** [get_recipient t] returns the recipient of transaction [t]*)

val verify_transaction : string -> t -> bool
(** [verify_transaction db t] verifies the validity of transaction within
    database file [db] and transaction [t]*)

val verify_transaction2 : t -> bool
(*val load_transactions : string -> t list*)

val create_transaction :
  string -> Account.account -> Account.account -> string -> string -> t
(** [create_transaction sender recipient amount sender_key] creates a
    transaction. [sender_key] must correspond to an account in the account
    database*)

val initialize_transaction_db : Sqlite3.header -> string -> unit
(**[initialize_transaction_db db sql] initializes the transaction database [db]
   with sql path [sql]*)

val add_transaction_to_db : Sqlite3.header -> t -> unit
(** [add_transactions_to_db db t] adds transaction [t] data into database [db] *)

val load_transactions : Sqlite3.header -> Sqlite3.header -> t list
(** [load_transactions dbt dba] loads the accounts within account file [dba] and
    transactions with [dbt] as a transaction list *)

val to_string_gui : t -> string
(** [to_string_gui t] returns transaction [t] as a list for gui *)

val clear_transaction_db : Sqlite3.header -> Sqlite3.header -> unit
(** [clear_transactions db1 db2] removed [db1] if it already exists and then
    initializes database [db2] *)
