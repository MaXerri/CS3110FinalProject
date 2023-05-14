(** [valid f] checks if the board size is a valid boardsize*)
let valid_size f =
  let i = String.split_on_char ' ' f in
  if List.length i <> 2 then false
  else if
    (match int_of_string_opt (List.nth i 0) with
    | Some num -> num
    | None -> -1)
    <= 0
    || (match int_of_string_opt (List.nth i 1) with
       | Some num -> num
       | None -> -1)
       <= 0
  then false
  else true

(** [play_game f] starts a minesweeper game of size f. *)
let play_game f =
  let i = String.split_on_char ' ' f in
  Minesweeper.State.init_state
    (int_of_string (List.nth i 0))
    (int_of_string (List.nth i 1))

(*Gets input for the boardsize*)
let rec get_input () =
  let x = read_line () in
  match valid_size x with
  | true -> play_game x
  | false ->
      print_endline "Invalid input. Try again";
      print_string "> ";
      get_input ()

(** let rec print_board_helper grid = match grid with | [] -> print_newline () |
    h :: t -> List.iter (fun elem -> match Minesweeper.Cell.to_char elem with |
    'X' -> print_string "! " | '_' -> print_string "_ " | '?' -> print_string "?
    " | _ -> print_string "error") h; print_newline (); print_board_helper t
    (*This has to be changed*) *)

(*new print_board_helper and the old one is above*)
let rec print_board_helper lst =
  match lst with
  | [] -> print_string ""
  | h :: t ->
      print_endline h;
      print_board_helper t

(*Function that turns tuple into string*)
let tuple_to_str t =
  let i =
    match t with
    | a, _ -> a
  in
  let j =
    match t with
    | _, b -> b
  in
  string_of_int i ^ " " ^ string_of_int j

(*retreives the next state based on a user input*)
let rec advance_game st =
  let x = read_line () in
  try
    let z = Minesweeper.Command.parse x in
    match z with
    | Minesweeper.Command.Clear n -> (
        match Minesweeper.State.clear (tuple_to_str n) st with
        | Minesweeper.State.Illegal ->
            print_endline "Invalid cell. Try again";
            print_string "> ";
            advance_game st
        | Minesweeper.State.Legal state -> state)
    | Minesweeper.Command.Flag n -> (
        match Minesweeper.State.flag (tuple_to_str n) st with
        | Minesweeper.State.Illegal ->
            print_endline "Invalid cell. Try again";
            print_string "> ";
            advance_game st
        | Minesweeper.State.Legal state -> state)
    | Minesweeper.Command.Quit ->
        print_endline "You Have Exitted The Game";
        exit 0
    | Minesweeper.Command.Help ->
        print_endline "Here is a list of the commands";
        print_endline "clear <row> <column>";
        print_endline "flag <row> <column>";
        print_endline "quit";
        print_endline "help";
        print_string "> ";
        advance_game st
    | Minesweeper.Command.Restart ->
        print_endline
          "You have restarted the game.  Input the board size for your new \
           game.";
        print_string "> ";
        let initial_state = get_input () in
        print_newline ();
        print_board_helper (*fix the printer helper*)
          (Minesweeper.Board.to_string_list
             (Minesweeper.State.get_current_board initial_state));
        print_newline ();
        print_endline "Enter a command";
        print_string "> ";
        advance_game initial_state
  with
  | Minesweeper.Command.Empty ->
      print_endline "Empty Input. Try Again";
      print_string "> ";
      advance_game st
  | Minesweeper.Command.Malformed ->
      print_endline "Malformed Input. Try Again";
      print_string "> ";
      advance_game st

(*Loops until the end of the game is reached*)
let rec progress st =
  let x = advance_game st in
  match Minesweeper.State.is_game_over x with
  | Minesweeper.State.Play ->
      print_board_helper
        (Minesweeper.Board.to_string_list
           (Minesweeper.State.get_current_board x));
      print_newline ();
      print_endline "Enter a command";
      print_string "> ";
      progress x
  | Minesweeper.State.Lost ->
      print_board_helper
        (Minesweeper.Board.to_string_list
           (Minesweeper.Board.uncover_board
              (Minesweeper.State.get_current_board x)));
      print_newline ();
      print_endline "You have Lost";
      print_endline "Enter the new game size you want to load";
      print_string "> ";
      let initial_state = get_input () in
      print_newline ();
      print_board_helper (*fix the printer helper*)
        (Minesweeper.Board.to_string_list
           (Minesweeper.State.get_current_board initial_state));
      print_newline ();
      print_endline "Enter a command";
      print_string "> ";
      progress initial_state
  | Minesweeper.State.Won -> print_endline "You Have Won"
(*This will need to be changed eventually*)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\nWelcome to Minesweeper \n";
  print_newline ();
  print_endline "These are the following commands and their descriptions:";
  print_endline
    "clear i j -> This means you choose to clear the cell at row i column j. \
     Rows and columns are both ZERO INDEXED";
  print_endline
    "flag i j -> This means you choose to toggle flagging the cell at row i \
     column j. Rows and columns are both ZERO INDEXED";
  print_endline
    "restart -> This starts a new game which you can select the size of";
  print_endline "quit -> This exits you out of the game";
  print_newline ();
  print_endline
    "Please enter the size of the game you want to load. Separate the desired \
     row and column size by 1 space";
  print_string "> ";
  let initial_state = get_input () in
  print_board_helper (*fix the printer helper*)
    (Minesweeper.Board.to_string_list
       (Minesweeper.State.get_current_board initial_state));
  print_newline ();
  print_endline "Enter a command";
  print_string "> ";
  progress initial_state

(* Execute the game engine. *)
let () = main ()
