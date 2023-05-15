(** [valid f] checks if the board size is a valid boardsize*)
let message_clear =
  "- [clear <row> <column>] -> Clears a single tile on the board.\n\
  \  If it contains a mine, you lose!"

let message_flag =
  "- [flag <row> <column>] -> Mark a single tile on the board so that\n\
  \  you can remember where you think a mine might be."

let message_rules =
  "- [rules] -> Pull up a menu with an explanation of the rules of \n\
  \  Minesweeper."

let message_help = "- [help] -> Pull up a menu of commands that you can input."

let message_help_sassy =
  message_help ^ " I'm not \n  sure how you got here without knowing this one."

let message_quit = "- [quit] -> Exit the game and return to the real world."

(*Retrieves user input in the form of "Yes", "yes", "Y", "y", "No", "no", "N",
  "n", and "quit" and converts it to a boolean value. Returns [false] in
  response to a "quit" command. Recurses on itself if an invalid input has been
  recieved*)
let rec binary_query () : bool = if true then true else binary_query ()

exception MalformedInput
exception TooSmallInput
exception OversizedBoard
exception NegativeSize

let valid_size f =
  let i = String.split_on_char ' ' f in
  if List.length i <> 2 then raise TooSmallInput
  else if
    (match int_of_string_opt (List.nth i 0) with
    | Some num -> if num < 27 then num else raise OversizedBoard
    | None -> raise MalformedInput)
    <= 0
    || (match int_of_string_opt (List.nth i 1) with
       | Some num -> if num < 27 then num else raise OversizedBoard
       | None -> raise MalformedInput)
       <= 0
  then raise NegativeSize
  else true

(** [play_game f] starts a minesweeper game of size f. *)
let play_game f =
  let i = String.split_on_char ' ' f in
  Minesweeper.State.init_state
    (int_of_string (List.nth i 0))
    (int_of_string (List.nth i 1))

(*Gets input for the boardsize*)
let rec get_input () =
  let re_call () =
    print_string "> ";
    get_input ()
  in
  let x = read_line () in
  try
    if valid_size x then play_game x
    else failwith "[get_input] impossible result"
  with
  | NegativeSize ->
      print_endline
        "At least one of your inputs was a negative number. Please try again:";
      re_call ()
  | OversizedBoard ->
      print_endline
        "You requested a board larger than (26 x 26). Please try again:";
      re_call ()
  | MalformedInput ->
      print_endline
        "At least one of the values you input was not a number. Please try \
         again:";
      re_call ()

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
        print_endline "You Have Exited The Game";
        exit 0
    | Minesweeper.Command.Help ->
        print_newline ();
        print_newline ();
        print_endline "Here are the commands available to you: \n";
        print_endline message_clear;
        print_endline message_flag;
        print_endline message_rules;
        print_endline message_help_sassy;
        print_endline message_quit;
        print_newline ();
        print_newline ();
        Minesweeper.(
          st |> State.get_current_board |> Board.to_string_list
          |> print_board_helper);
        print_newline ();
        print_endline "Enter a command";
        print_string "> ";
        advance_game st
    | Minesweeper.Command.Rules ->
        print_newline ();
        print_newline ();
        print_endline
          "The objective of Minesweeper is to clear the game board without\n\
           triggering any of the hidden mines. The board itself consists of \n\
           a rectangular grid of cells, some of which are empty, some of which\n\
           contain mines, and some of which contain a number corresponding to \n\
           the number of mines adjacent to them (including diagonally).\n\n\
           You must use the information revealed by the visible cells to \n\
           deduce which cells are safe to clear, and which cells are not.\n\n\
           To view a list of the commands you have at your disposal, enter \n\
           the command \"help\".\n\n\
           Happy Sweeping!";
        print_newline ();
        print_newline ();
        Minesweeper.(
          st |> State.get_current_board |> Board.to_string_list
          |> print_board_helper);
        print_newline ();
        print_endline "Enter a command";
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
      print_endline "Would you like to try again?";
      if binary_query () then call_newGame () else ()
  | Minesweeper.State.Won ->
      print_endline "You Have Won!";
      print_endline "Would you like to try again?";
      if binary_query () then call_newGame () else ()

and call_newGame () =
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

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\n\n\n\n\nWelcome to Minesweeper! \n";
  print_newline ();
  print_endline "Here is a list of the commands you have at your disposal:";
  print_endline message_clear;
  print_endline message_flag;
  print_endline message_rules;
  print_endline message_help;
  print_endline message_quit;
  print_newline ();
  print_endline
    "Please enter the size of the game you want to load. Separate the desired\n\
     row and column size by 1 space. The maximum available board size is \n\
     (26 x 26): \n";
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
