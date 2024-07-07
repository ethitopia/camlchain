open Raylib
open Raygui
open GroupProject

type state = {
  mutable account_name_rec_text : string;
  mutable account_name_rec_active : bool;
  mutable account_name_rejection : bool;
  mutable balance_rec_text : string;
  mutable balance_rec_active : bool;
  mutable balance_rejection : bool;
  mutable public_key_screen : bool;
  mutable public_key_text : string;
  mutable public_key_text_empty : bool;
  mutable public_key_non_unique : bool;
  mutable public_key : string;
  mutable activate_account : bool;
  mutable activate_account_screen : bool;
  mutable account_name : string;
  mutable balance : int;
  mutable view_account_list_view_ex_index : int;
  mutable view_account_list_view_ex_focus : int;
  mutable view_account_list_view_ex_active : int;
  mutable view_account_search_text : string;
  mutable view_account_search_edit_mode : bool;
  mutable view_transactions_search_text : string;
  mutable view_transactions_search_edit_mode : bool;
  mutable view_transactions_list_view_ex_focus : int;
  mutable view_transactions_list_view_ex_active : int;
  mutable view_transactions_list_view_ex_index : int;
  mutable sender : string;
  mutable recipient : string;
  mutable amount : int;
  mutable start_mine_menu : bool;
  mutable mining_phase : bool;
  mutable account_name_duplicate : bool;
  mutable recipient_active : bool;
  mutable amount_active : bool;
  mutable create_transaction_active : bool;
  mutable id_not_match : bool;
  mutable mining_end : bool;
  mutable blockchain : Blockchain.t;
  mutable public_key_rejection : bool;
  mutable balance_low : bool;
}

let db_path = ref "data/accounts.db"
let db_pending_transaction_path = ref "data/pending_transactions.db"
let db_transactions_path = ref "data/transactions.db"
let sql_script_path = ref "data/init_db.sql"

let sql_script_pending_transactions_path =
  ref "data/init_transaction_pool_db.sql"

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with Not_found -> false

let strip_text_box_string str =
  try String.sub str 0 (String.index str '\000') with _ -> str

let create_start_state () =
  {
    balance_low = false;
    public_key_rejection = false;
    blockchain = Blockchain.init ();
    mining_end = true;
    id_not_match = false;
    create_transaction_active = true;
    amount_active = false;
    recipient_active = false;
    account_name_duplicate = false;
    account_name_rec_text = "";
    account_name_rec_active = true;
    account_name_rejection = false;
    balance_rec_text = "";
    balance_rec_active = false;
    balance_rejection = false;
    public_key_screen = true;
    public_key_text = "";
    public_key_text_empty = false;
    public_key_non_unique = false;
    public_key = "";
    sender = "";
    recipient = "";
    amount = 0;
    activate_account = true;
    activate_account_screen = false;
    account_name = "";
    balance = 0;
    view_account_list_view_ex_index = 0;
    view_account_list_view_ex_focus = 0;
    view_account_list_view_ex_active = 0;
    view_account_search_text = "";
    view_account_search_edit_mode = false;
    view_transactions_search_text = "";
    view_transactions_search_edit_mode = false;
    view_transactions_list_view_ex_focus = 0;
    view_transactions_list_view_ex_index = 0;
    view_transactions_list_view_ex_active = 0;
    start_mine_menu = true;
    mining_phase = true;
  }

let setup () =
  init_window 1200 1000 "test";
  set_target_fps 60;
  create_start_state ()

(**[mouse_collision_rectangle_operation rectangle new_color textfunction
   leftclickfunction]
   checks to see if mouse position collides with rectangle. If it does, then it
   redraws the rectangle with new_color and performs the textfunction. If there
   is a left click inside the rectangle, then performs leftclickfunction*)
let mouse_collision_rectangle_operation rectangle new_color textfunction
    leftclickfunction =
  if check_collision_point_rec (get_mouse_position ()) rectangle then (
    draw_rectangle_rec rectangle new_color;
    textfunction ();
    if is_mouse_button_pressed MouseButton.Left then leftclickfunction ())

let rec add_wallet_menu (state : state) =
  if window_should_close () then close_window ()
  else (
    begin_drawing ();
    clear_background Color.gray;
    draw_text
      ("(x,y): ("
      ^ string_of_int (get_mouse_x ())
      ^ ", "
      ^ string_of_int (get_mouse_y ())
      ^ ")")
      0 0 10 Color.black;

    draw_text "Create an Account" 80 150 50 Color.red;

    if state.account_name_rec_active then (
      draw_text "Enter Account Name. Must not already exist" 250 360 40
        Color.black;

      if state.account_name_rejection then
        draw_text "Please enter a non empty name" 360 560 30 Color.red;
      if state.account_name_duplicate then
        draw_text "Name already exists. Please enter another name" 360 560 30
          Color.red;

      let enter_account_name_rec = Rectangle.create 360. 420. 360. 100. in
      draw_rectangle_rec enter_account_name_rec Color.white;
      let account_name_input, account_name_clicked =
        text_input_box enter_account_name_rec "" "" "Enter"
          state.account_name_rec_text
      in
      state.account_name_rec_text <- account_name_input;
      if account_name_clicked > 0 then
        if
          String.sub account_name_input 0
            (String.index account_name_input '\000')
          = ""
        then (
          state.account_name_rejection <- true;
          state.account_name_duplicate <- false)
        else if
          Account.verify_account_by_name
            (strip_text_box_string state.account_name_rec_text)
            !db_path
        then (
          state.account_name_rejection <- false;
          state.account_name_duplicate <- true)
        else (
          state.account_name <-
            String.sub account_name_input 0
              (String.index account_name_input '\000');
          state.account_name_rec_active <- false;
          state.balance_rec_active <- true))
    else if state.balance_rec_active then (
      draw_text "Enter a Starting Balance as an Integer" 360 360 40 Color.black;

      if state.balance_rejection then
        draw_text "Please enter an integer" 360 560 30 Color.red;

      let enter_balance_rec = Rectangle.create 360. 420. 360. 100. in
      draw_rectangle_rec enter_balance_rec Color.white;

      let balance_input, balance_clicked =
        text_input_box enter_balance_rec "" "" "Enter" state.balance_rec_text
      in
      state.balance_rec_text <- balance_input;
      if balance_clicked > 0 then
        try
          state.balance <-
            int_of_string
              (String.sub balance_input 0 (String.index balance_input '\000'));
          state.balance_rejection <- false;
          state.balance_rec_active <- false;
          state.public_key_screen <- true
        with Failure _ -> state.balance_rejection <- true)
    else if state.public_key_screen then (
      draw_text "Enter an integer key id that" 100 250 40 Color.black;
      draw_text "will be used to identify your account, and is private" 100 310
        40 Color.black;
      draw_text "This will have to be a unique non empty id" 100 370 40
        Color.black;

      if state.public_key_text_empty then
        draw_text "Please enter a non empty ID" 360 560 30 Color.red;
      if state.public_key_rejection then
        draw_text "Please enter an integer" 360 560 30 Color.red;

      if state.public_key_non_unique then
        draw_text "ID already exists. Please enter another ID" 360 560 30
          Color.red;

      let enter_key_id_rec = Rectangle.create 360. 420. 360. 100. in
      draw_rectangle_rec enter_key_id_rec Color.white;

      let public_key_text, public_key_button_clicked =
        text_input_box enter_key_id_rec "" "" "Enter" state.public_key_text
      in

      state.public_key_text <- public_key_text;
      if public_key_button_clicked > 0 then (
        if
          String.sub public_key_text 0 (String.index public_key_text '\000')
          = ""
        then (
          state.public_key_text_empty <- true;
          state.public_key_non_unique <- false)
        else if
          Account.verify_account_by_public_key
            (strip_text_box_string state.public_key)
            !db_path
        then (
          state.public_key_non_unique <- true;
          state.public_key_rejection <- false)
        else
          try
            ignore (int_of_string (strip_text_box_string state.public_key_text));
            state.public_key <-
              String.sub public_key_text 0 (String.index public_key_text '\000');
            state.public_key_screen <- false;
            state.activate_account_screen <- true;
            state.public_key_rejection <- false
          with _ ->
            state.public_key_rejection <- true;
            state.public_key_text_empty <- false;
            state.public_key_non_unique <- false))
    else if state.activate_account_screen then
      if state.activate_account then (
        let account =
          Account.create_account_object
            (strip_text_box_string state.account_name)
            (int_of_string
               (strip_text_box_string (string_of_int state.balance)))
            (strip_text_box_string state.public_key)
        in
        Account.add_account !db_path account;
        state.activate_account <- false)
      else (
        draw_text
          (Printf.sprintf "Account has been made with the name %s"
             state.account_name)
          160 360 40 Color.black;

        draw_text
          (Printf.sprintf "with a starting balance of %i" state.balance)
          160 400 40 Color.black;

        draw_text
          (Printf.sprintf "and with a id of %s" state.public_key)
          160 440 40 Color.black;

        draw_text
          (Printf.sprintf "Keep this id private to you only, and remember it.")
          160 580 40 Color.black;

        draw_text "This will be used to sign outgoing transactions" 160 620 40
          Color.black);

    let go_back_to_menu_rec = Rectangle.create 300. 700. 360. 50. in

    draw_rectangle_rec go_back_to_menu_rec Color.blue;
    draw_text "Go back to main menu" 305 710 30 Color.black;

    mouse_collision_rectangle_operation go_back_to_menu_rec Color.darkblue
      (fun () -> draw_text "Go back to main menu" 305 710 30 Color.black)
      (fun () ->
        end_drawing ();
        loop_menu (create_start_state ()));

    end_drawing ();

    add_wallet_menu state)

and create_transaction_menu (state : state) =
  if window_should_close () then close_window ()
  else (
    begin_drawing ();
    clear_background Color.gray;
    draw_text
      ("(x,y): ("
      ^ string_of_int (get_mouse_x ())
      ^ ", "
      ^ string_of_int (get_mouse_y ())
      ^ ")")
      0 0 10 Color.black;

    draw_text "Create a transaction" 80 150 50 Color.red;

    if state.account_name_rec_active then (
      draw_text "Enter Sender Name" 360 360 40 Color.black;

      if state.account_name_rejection then
        draw_text "Please enter a non empty name" 360 560 30 Color.red;
      if state.account_name_duplicate then
        draw_text "Name already exists. Please enter another name" 360 560 30
          Color.red;

      let enter_sender_rec = Rectangle.create 360. 420. 360. 100. in
      draw_rectangle_rec enter_sender_rec Color.white;

      let sender_input, account_name_clicked =
        text_input_box enter_sender_rec "" "" "Enter" state.sender
      in
      state.sender <- sender_input;
      if account_name_clicked > 0 then
        if account_name_clicked > 0 then
          if strip_text_box_string sender_input = "" then (
            state.account_name_rejection <- true;
            state.account_name_duplicate <- false)
          else if
            not
              (Account.verify_account_by_name
                 (strip_text_box_string state.sender)
                 !db_path)
          then (
            state.account_name_rejection <- false;
            state.account_name_duplicate <- true)
          else (
            state.sender <-
              String.sub sender_input 0 (String.index sender_input '\000');
            state.account_name_rec_active <- false;
            state.recipient_active <- true))
    else if state.recipient_active then (
      draw_text "Enter Recipient" 360 360 40 Color.black;

      if state.account_name_rejection then
        draw_text "Please enter a non empty name" 360 560 30 Color.red;
      if state.account_name_duplicate then
        draw_text "Name already exists. Please enter another name" 360 560 30
          Color.red;

      let enter_transaction_recipient_rec =
        Rectangle.create 360. 420. 360. 100.
      in
      draw_rectangle_rec enter_transaction_recipient_rec Color.white;

      let recipient_input, recipient_clicked =
        text_input_box enter_transaction_recipient_rec "" "" "Enter"
          state.recipient
      in
      state.recipient <- strip_text_box_string recipient_input;
      if recipient_clicked > 0 then
        if strip_text_box_string recipient_input = "" then (
          state.account_name_rejection <- true;
          state.account_name_duplicate <- false)
        else if
          not
            (Account.verify_account_by_name
               (strip_text_box_string recipient_input)
               !db_path)
        then (
          state.account_name_rejection <- false;
          state.account_name_duplicate <- true)
        else (
          state.recipient_active <- false;
          state.amount_active <- true))
    else if state.amount_active then (
      draw_text "Enter amount" 360 360 40 Color.black;

      if state.balance_rejection then
        draw_text "Please enter an integer" 360 560 30 Color.red;
      if state.balance_low then
        draw_text "Sender's balance too low" 360 560 30 Color.red;

      let enter_balance_rec = Rectangle.create 360. 420. 360. 100. in
      draw_rectangle_rec enter_balance_rec Color.white;

      let balance_input, balance_clicked =
        text_input_box enter_balance_rec "" "" "Enter" state.balance_rec_text
      in
      state.balance_rec_text <- balance_input;

      if balance_clicked > 0 then (
        try
          state.amount <-
            int_of_string
              (String.sub balance_input 0 (String.index balance_input '\000'));
          if state.amount < 0 then (
            state.balance_rejection <- true;
            state.balance_low <- false)
          else (
            state.balance_rejection <- false;
            state.amount_active <- false;
            state.public_key_screen <- true)
        with Failure _ ->
          state.balance_rejection <- true;
          state.balance_low <- false))
    else if state.public_key_screen then (
      draw_text "Enter your account ID" 360 360 40 Color.black;

      if state.id_not_match then
        draw_text "ID does not match senders name" 360 560 30 Color.red;

      let enter_transaction_recipient_rec =
        Rectangle.create 360. 420. 360. 100.
      in
      draw_rectangle_rec enter_transaction_recipient_rec Color.white;

      let key_input, key_clicked =
        text_input_box enter_transaction_recipient_rec "" "" "Enter"
          state.public_key
      in
      state.public_key <- strip_text_box_string key_input;
      if key_clicked > 0 then
        if strip_text_box_string key_input = "" then (
          state.account_name_rejection <- true;
          state.account_name_duplicate <- false)
        else (
          state.public_key_screen <- false;
          state.activate_account_screen <- true))
    else if state.activate_account_screen then
      if state.create_transaction_active then (
        let senderacc =
          match
            Account.get_account_by_name !db_path
              (strip_text_box_string state.sender)
          with
          | Some account -> account
          | None -> failwith "Account not found for sender"
        in
        if Account.get_balance senderacc < state.amount then (
          state.activate_account_screen <- false;
          state.amount_active <- true;
          state.balance_low <- true)
        else
          let recipientacc =
            match Account.get_account_by_name !db_path state.recipient with
            | Some account -> account
            | None -> failwith "Account not found for recipient"
          in
          try
            let new_transaction =
              Transaction.create_transaction !db_path senderacc recipientacc
                (string_of_int state.amount)
                state.public_key
            in

            Transaction.add_transaction_to_db
              !db_pending_transaction_path
              new_transaction;
            state.create_transaction_active <- false;
            state.id_not_match <- false;
            state.activate_account_screen <- true
          with _ ->
            state.activate_account_screen <- false;
            state.public_key_screen <- true;
            state.id_not_match <- true)
      else (
        draw_text
          (Printf.sprintf "Transaction has been made with sender %s,"
             state.sender)
          160 360 40 Color.black;

        draw_text
          (Printf.sprintf "recipient %s and the amount as %i" state.recipient
             state.amount)
          160 400 40 Color.black);
    let go_back_to_menu_rec = Rectangle.create 300. 600. 360. 50. in

    draw_rectangle_rec go_back_to_menu_rec Color.blue;
    draw_text "Go back to main menu" 305 610 30 Color.black;

    mouse_collision_rectangle_operation go_back_to_menu_rec Color.darkblue
      (fun () -> draw_text "Go back to main menu" 305 610 30 Color.black)
      (fun () ->
        end_drawing ();
        loop_menu (create_start_state ()));

    end_drawing ();

    create_transaction_menu state)

and view_transactions_menu state =
  if window_should_close () then close_window ()
  else (
    begin_drawing ();
    clear_background Color.gray;
    draw_text
      ("(x,y): ("
      ^ string_of_int (get_mouse_x ())
      ^ ", "
      ^ string_of_int (get_mouse_y ())
      ^ ")")
      0 0 10 Color.black;
    draw_text "View Transactions" 80 100 50 Color.red;

    draw_text "Enter the sender's name to search list" 360 160 30 Color.black;

    let text_box_rec = Rectangle.create 360. 200. 460. 30. in
    draw_rectangle_rec text_box_rec Color.white;
    let view_transactions_search_text =
      match
        text_box text_box_rec state.view_transactions_search_text
          state.view_transactions_search_edit_mode
      with
      | vl, true ->
          state.view_transactions_search_edit_mode <-
            not state.view_transactions_search_edit_mode;
          vl
      | vl, false -> vl
    in
    state.view_transactions_search_text <-
      (try
         String.sub view_transactions_search_text 0
           (String.index view_transactions_search_text '\000')
       with Not_found -> view_transactions_search_text);
    let transactions_list =
      Transaction.load_transactions !db_transactions_path !db_path
    in

    let string_transactions_list =
      ref (List.rev (List.map Transaction.to_string_gui transactions_list))
    in
    if state.view_transactions_search_text <> "" then
      string_transactions_list :=
        List.filter
          (fun x ->
            contains x ("Sender: " ^ state.view_transactions_search_text))
          !string_transactions_list;
    let list_rectangle = Rectangle.create 360. 250. 460. 500. in
    let active, focus, item_index =
      Raygui.list_view_ex list_rectangle !string_transactions_list
        state.view_transactions_list_view_ex_focus
        state.view_transactions_list_view_ex_index
        state.view_transactions_list_view_ex_active
    in
    state.view_transactions_list_view_ex_focus <- focus;
    state.view_transactions_list_view_ex_active <- active;
    state.view_transactions_list_view_ex_index <- item_index;

    let go_back_to_menu_rec = Rectangle.create 300. 850. 360. 50. in

    draw_rectangle_rec go_back_to_menu_rec Color.blue;
    draw_text "Go back to main menu" 305 860 30 Color.black;

    mouse_collision_rectangle_operation go_back_to_menu_rec Color.darkblue
      (fun () -> draw_text "Go back to main menu" 305 860 30 Color.black)
      (fun () ->
        end_drawing ();
        loop_menu (create_start_state ()));

    end_drawing ();
    view_transactions_menu state)

and view_account_menu state =
  if window_should_close () then close_window ()
  else (
    begin_drawing ();
    clear_background Color.gray;
    draw_text
      ("(x,y): ("
      ^ string_of_int (get_mouse_x ())
      ^ ", "
      ^ string_of_int (get_mouse_y ())
      ^ ")")
      0 0 10 Color.black;
    draw_text "View Account" 80 150 50 Color.red;

    draw_text "Enter account name to search list" 360 250 30 Color.black;
    let text_box_rec = Rectangle.create 360. 310. 460. 30. in
    draw_rectangle_rec text_box_rec Color.white;
    let view_account_search_text =
      match
        text_box text_box_rec state.view_account_search_text
          state.view_account_search_edit_mode
      with
      | vl, true ->
          state.view_account_search_edit_mode <-
            not state.view_account_search_edit_mode;
          vl
      | vl, false -> vl
    in
    state.view_account_search_text <-
      (try
         String.sub view_account_search_text 0
           (String.index view_account_search_text '\000')
       with Not_found -> view_account_search_text);
    let account_list = Account.load_accounts !db_path in
    let string_account_list =
      ref (List.rev (Account.to_string_gui_list account_list))
    in
    if state.view_account_search_text <> "" then
      string_account_list :=
        List.filter
          (fun x -> contains x ("Name: " ^ state.view_account_search_text))
          !string_account_list;
    let list_rectangle = Rectangle.create 360. 380. 460. 500. in
    let active, focus, item_index =
      Raygui.list_view_ex list_rectangle !string_account_list
        state.view_account_list_view_ex_focus
        state.view_account_list_view_ex_index
        state.view_account_list_view_ex_active
    in
    state.view_account_list_view_ex_focus <- focus;
    state.view_account_list_view_ex_active <- active;
    state.view_account_list_view_ex_index <- item_index;

    let go_back_to_menu_rec = Rectangle.create 300. 950. 360. 50. in

    draw_rectangle_rec go_back_to_menu_rec Color.blue;
    draw_text "Go back to main menu" 305 950 30 Color.black;

    mouse_collision_rectangle_operation go_back_to_menu_rec Color.darkblue
      (fun () -> draw_text "Go back to main menu" 305 950 30 Color.black)
      (fun () ->
        end_drawing ();
        loop_menu (create_start_state ()));

    end_drawing ();
    view_account_menu state)

and mine_transactions_menu state =
  if window_should_close () then close_window ()
  else (
    begin_drawing ();
    clear_background Color.gray;
    draw_text
      ("(x,y): ("
      ^ string_of_int (get_mouse_x ())
      ^ ", "
      ^ string_of_int (get_mouse_y ())
      ^ ")")
      0 0 10 Color.black;
    if state.start_mine_menu then (
      draw_text "Mine Transactions" 20 150 50 Color.red;

      draw_text
        "This menu is for mining all of the currently pending transactions \
         into the blockchain"
        20 220 25 Color.black;

      draw_text
        "Below you can see all of the pending transactions that still must be \
         mined."
        20 250 25 Color.black;

      draw_text "Press the \"Mine Transactions\" button to start mining." 20 280
        25 Color.black;

      draw_text "Keep in mind that this process will likely take some time." 20
        310 25 Color.black;

      let pending_transaction_list =
        Transaction.load_transactions !db_pending_transaction_path !db_path
      in
      let string_account_list =
        ref
          (List.rev
             (List.map Transaction.to_string_gui pending_transaction_list))
      in
      if state.view_account_search_text <> "" then
        string_account_list :=
          List.filter
            (fun x -> contains x ("ID: " ^ state.view_account_search_text))
            !string_account_list;
      let list_rectangle = Rectangle.create 360. 340. 460. 500. in
      let active, focus, item_index =
        Raygui.list_view_ex list_rectangle !string_account_list
          state.view_account_list_view_ex_focus
          state.view_account_list_view_ex_index
          state.view_account_list_view_ex_active
      in
      state.view_account_list_view_ex_focus <- focus;
      state.view_account_list_view_ex_active <- active;
      state.view_account_list_view_ex_index <- item_index;

      let mine_blockchain_rec = Rectangle.create 300. 850. 360. 50. in

      draw_rectangle_rec mine_blockchain_rec Color.blue;
      draw_text "Mine Blockchain" 305 860 30 Color.black;
      if not (List.is_empty pending_transaction_list) then
        mouse_collision_rectangle_operation mine_blockchain_rec Color.darkblue
          (fun () -> draw_text "Mine Blockchain" 305 860 30 Color.black)
          (fun () -> state.start_mine_menu <- false))
    else if state.mining_phase then (
      draw_text "Now Mining Transactions..." 20 150 50 Color.red;
      end_drawing ();
      state.blockchain <-
        Blockchain.(
          add_block state.blockchain
            (mine_block
               (create_block
                  (Transaction.load_transactions !db_transactions_path !db_path)
                  "0"
                  (get_hash (get_last_block state.blockchain))
                  "")
               5));
      let pending_transaction_list =
        Transaction.load_transactions !db_pending_transaction_path !db_path
      in
      List.iter
        (Transaction.add_transaction_to_db !db_transactions_path)
        pending_transaction_list;
      Transaction.clear_transaction_db
        !db_pending_transaction_path
        !sql_script_pending_transactions_path;
      List.iter
        (fun x ->
          Account.update_balance !db_path
            (Account.get_name (Transaction.get_sender x))
            (int_of_string (Transaction.get_amount x))
            false;
          Account.update_balance !db_path
            (Account.get_name (Transaction.get_recipient x))
            (int_of_string (Transaction.get_amount x))
            true)
        pending_transaction_list;
      state.mining_phase <- false;
      state.mining_end <- true)
    else if state.mining_end then (
      begin_drawing ();
      draw_text "Mining has completed. Thank you for mining the transactions :)"
        20 150 30 Color.black);
    let go_back_to_menu_rec = Rectangle.create 300. 950. 360. 50. in

    draw_rectangle_rec go_back_to_menu_rec Color.blue;
    draw_text "Go back to main menu" 305 960 30 Color.black;

    mouse_collision_rectangle_operation go_back_to_menu_rec Color.darkblue
      (fun () -> draw_text "Go back to main menu" 305 960 30 Color.black)
      (fun () ->
        end_drawing ();
        loop_menu (create_start_state ()));

    end_drawing ();
    mine_transactions_menu state)

and loop_menu (state : state) =
  if window_should_close () then close_window ()
  else (
    begin_drawing ();
    clear_background Color.gray;

    draw_text
      ("(x,y): ("
      ^ string_of_int (get_mouse_x ())
      ^ ", "
      ^ string_of_int (get_mouse_y ())
      ^ ")")
      0 0 10 Color.black;

    let mine_rec = Rectangle.create 340. 600. (700. -. 340.) (990. -. 940.) in
    let view_accounts_rec =
      Rectangle.create 340. 500. (700. -. 340.) (890. -. 840.)
    in
    let view_transactions_rec =
      Rectangle.create 340. 400. (700. -. 340.) (790. -. 740.)
    in
    let create_transactions_rec =
      Rectangle.create 340. 300. (700. -. 340.) (690. -. 640.)
    in
    let add_wallet_rec =
      Rectangle.create 340. 200. (700. -. 340.) (790. -. 740.)
    in
    let exit_rectangle =
      Rectangle.create 340. 700. (700. -. 340.) (1090. -. 1040.)
    in

    draw_text "OCaml Blockchain Simulator" 235 30 60 Color.red;
    draw_text "What do you wish to do?" 235 100 30 Color.black;

    draw_rectangle_rec add_wallet_rec Color.blue;
    draw_text "Create an Account" 360 205 30 Color.black;

    draw_rectangle_rec create_transactions_rec Color.blue;
    draw_text "Create a Transaction" 360 305 30 Color.black;

    draw_rectangle_rec view_transactions_rec Color.blue;
    draw_text "View Transactions" 360 405 30 Color.black;

    draw_rectangle_rec view_accounts_rec Color.blue;
    draw_text "View Accounts" 360 505 30 Color.black;

    draw_rectangle_rec mine_rec Color.blue;
    draw_text "Mine Transactions" 360 605 30 Color.black;

    draw_rectangle_rec exit_rectangle Color.blue;
    draw_text "Exit" 360 705 30 Color.black;

    (*rec operations*)
    mouse_collision_rectangle_operation exit_rectangle Color.darkblue
      (fun () -> draw_text "Exit" 360 705 30 Color.black)
      close_window;

    mouse_collision_rectangle_operation add_wallet_rec Color.darkblue
      (fun () -> draw_text "Create an Account" 360 205 30 Color.black)
      (fun () ->
        end_drawing ();
        add_wallet_menu state);
    mouse_collision_rectangle_operation create_transactions_rec Color.darkblue
      (fun () -> draw_text "Create a Transaction" 360 305 30 Color.black)
      (fun () ->
        end_drawing ();
        create_transaction_menu state);
    mouse_collision_rectangle_operation view_transactions_rec Color.darkblue
      (fun () -> draw_text "View Transactions" 360 405 30 Color.black)
      (fun () ->
        end_drawing ();
        view_transactions_menu state);
    mouse_collision_rectangle_operation mine_rec Color.darkblue
      (fun () -> draw_text "Mine Transactions" 360 605 30 Color.black)
      (fun () ->
        end_drawing ();
        mine_transactions_menu state);
    mouse_collision_rectangle_operation view_accounts_rec Color.darkblue
      (fun () -> draw_text "View Accounts" 360 505 30 Color.black)
      (fun () ->
        end_drawing ();
        view_account_menu state);

    end_drawing ();
    loop_menu state)

let () =
  Account.initialize_db !db_path !sql_script_path;
  Transaction.initialize_transaction_db
    !db_pending_transaction_path
    !sql_script_pending_transactions_path;
  Transaction.initialize_transaction_db !db_transactions_path
    !sql_script_pending_transactions_path
  |> setup |> loop_menu
