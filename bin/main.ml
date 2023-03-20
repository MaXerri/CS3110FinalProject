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
  Minesweeper.Board.generate
    (int_of_string (List.nth i 0))
    (int_of_string (List.nth i 1))

let rec repeat () =
  print_endline "Input a valid size";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name ->
      if valid_size file_name = false then repeat () else play_game file_name

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline
    "Please enter the size of the game you want to load. Separate the desired \
     row and column size by 1 space";
  print_string "> ";

  match read_line () with
  | exception End_of_file -> ()
  | file_name ->
      if valid_size file_name = false then repeat () else play_game file_name

(* Execute the game engine. *)
let () = main ()
