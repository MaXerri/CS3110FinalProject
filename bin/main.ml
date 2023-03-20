(** [valid f] checks if the board size is a valid boardsize*)
let valid_size f =
  let i = String.split_on_char ' ' f in
  if List.length i <> 2 then false
  else if
    (match int_of_string_opt (List.nth i 0) with
    | Some num -> num
    | None -> -1)
    < 0
    || (match int_of_string_opt (List.nth i 1) with
       | Some num -> num
       | None -> -1)
       < 0
  then false
  else true

(** [play_game f] starts a minesweeper game of size f. *)
let play_game f =
  let i = String.split_on_char ' ' f in
  Minesweeper.State.init_state
    (int_of_string (List.nth i 0))
    (int_of_string (List.nth i 1))

let rec get_input () =
  let x = read_line () in 
  match valid_size (x) with
  | true -> play_game x
  | false -> (print_string "Invalid input. Try again"; get_input ()) (*This has to be changed*)

let rec print_board_helper st = 
 match (Minesweeper.State.get_current_board st) with
 | [] -> print_newline ();
 | h::t -> let () = Array.iter (fun elem -> print_string ("X ")) h; print_board_helper t;


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline
    "Please enter the size of the game you want to load. Separate the desired \
     row and column size by 1 space";
  print_string "> ";

 let initial_state = get_input in 
  while (not (Minesweeper.State.is_game_over initial_state)) do 
    print_board_helper initial_state;
    print_endline "Clear Another Square";
  done 
  
(* Execute the game engine. *)
let () = main ()
