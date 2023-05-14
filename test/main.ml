(*Test plan (By Module): *)
(**bin/main file: This was tested entirely based on manual testing by playing
   the game to make sure that we were prompting the user in the correct order
   and correct messages were displaying on the screne at the right time. *)
(*State: This file was blackbox tested and glassbox tested through ounit test
  cases and also we verified everything worked manually by playing the game to
  check if edge cases worked and that different parttern matching branches were
  working properly. We would generate initial states based off a certain board
  starting connfiguration and then we would prompt moves in the game to make new
  states. Making the new states helped blackbox coverage as we could trigger
  endgame states and simulate midgame states as well. Also make tests based on
  specification to ensure edge cases were working and then also looked through
  the code for these respective functions to make sure that all branches were
  being tested *)
(*Board: This file was tested using blackbox testing and manual testing by
  playing the game. Black box testing was used to ensure that the unction was
  behaving with repect to the specification and the tests we made ensured that
  commands to clear and flag matched the printed out boards and that also we
  checked if certain boards should trigger end states. The majority of the
  testing was done manually though through testing using make play and playing
  the game. We were feeding in fixed boards so we know where the bombds were and
  then we tested clearing and flagging different squares to enure the correct
  functionality. *)
(*Cell: This file was tested using glassbox and blackbox testing. We checked the
  branches of the functions and tested every one and also made tests to ensure
  that edge cases on the inputs to the functions were covered. We ensured that
  cells were displaying correctly by themselves depending if they were visible
  or not and iwhat their underlying cell type. *)
(*Commmand: This file was tested using glassbox, and manual testing. This files
  handles invalid inputs so there was a lot of pattern matches to invalid inputs
  and glassbox testing was used to ensures all these branches worked correctly.
  Additionally, we vigorously were trying to catch errors by playing the game
  and putting in invalid inputs to ensure that our game caught these and
  prompted the user to put in a new input that would be valid*)
(*OVERALL CORRECTNESS: First, State encompasses board and cell in a way as state
  contains a board and a board is a matrix of cells. We were able to show the
  correctness of the cell data type first through glass and backbox testing and
  trial and error of playing the game. We have ensures that the smallest unit of
  the game is working. Then we tested board and its functions and we know that
  cell is correct as well so we have proved correctness for board. The same
  logic goes into proving the correctness of state which encompasses a board.
  testing the correctness of State shows that the progression of the game will
  work correctly and that endstates are detected when reached and the game
  progresses correctly. Then lastly showing correctness of the command parsing
  shows that we will be able to correctly progress the game through user inputs.
  The command is connected to state as it is what allows the user to interact
  with game and advance the state of the game. Thus thorugh out test regime we
  have shown the correctness of the entre system *)

open OUnit2
open Minesweeper

(* open Board *)
open Cell

(* open Command *)
(* open State *)

(*let cmp_cells (cell1 : cell) (cell2 : cell) = true*)

(*temporary cells for testing purposes*)
let mineCell = generate (-1)
let emptyCell = generate 0
let v1Cell = generate 1
let v2Cell = generate 2
let v3Cell = generate 3
let v4Cell = generate 4
let v5Cell = generate 5
let v6Cell = generate 6
let v7Cell = generate 7
let v8Cell = generate 8

(*let badCell = generate (-2)*)
(*let maxIntCell = generate 1073741823*)
(*let minIntCell = generate (-1073741824)*)

(*Boolean Board testing*)
let bg_small = [ [ true; false ]; [ false; false ] ]
let bg_small_slist = [ "A  X X "; "B  X X "; ""; "   A B " ]

(*switches the arguments of Board.clear_position to allow for pipelining*)
let clear_rev t b = Board.clear_position b t
let flag_rev t b = Board.flag_position b t

(*2d list printer*)
let rec pp_lst lst =
  match lst with
  | [] -> "]"
  | h :: t -> h ^ "; " ^ pp_lst t

let bb2 =
  let bb_int_1 =
    [ [ 1; 0; 0; 0 ]; [ 0; 1; 0; 0 ]; [ 0; 0; 1; 0 ]; [ 0; 0; 0; 1 ] ]
  in
  List.map (List.map (fun a -> a = 1)) bb_int_1

let bb_max_adjacent_bombs =
  let x = [ [ 1; 1; 1; 0 ]; [ 1; 0; 1; 0 ]; [ 1; 1; 1; 0 ]; [ 0; 0; 0; 0 ] ] in
  List.map (List.map (fun a -> a = 1)) x

let b_start = Board.generate_from_bool_grid bb2

let b_won =
  Board.generate_from_bool_grid bb2
  |> clear_rev (0, 1)
  |> clear_rev (0, 2)
  |> clear_rev (0, 3)
  |> clear_rev (1, 0)
  |> clear_rev (1, 2)
  |> clear_rev (1, 3)
  |> clear_rev (2, 0)
  |> clear_rev (2, 1)
  |> clear_rev (2, 3)
  |> clear_rev (3, 0)
  |> clear_rev (3, 1)
  |> clear_rev (3, 2)

let b_not_complete =
  Board.generate_from_bool_grid bb2
  |> clear_rev (0, 1)
  |> clear_rev (0, 2)
  |> clear_rev (0, 3)
  |> clear_rev (1, 2)

let b_not_valid =
  Board.generate_from_bool_grid bb2
  |> clear_rev (0, 1)
  |> clear_rev (0, 2)
  |> clear_rev (0, 3)
  |> clear_rev (1, 1)

let b_max_adjacent_bombs =
  Board.generate_from_bool_grid bb_max_adjacent_bombs |> clear_rev (1, 1)

let b_with_1_flag =
  Board.generate_from_bool_grid bb2
  |> clear_rev (0, 1)
  |> clear_rev (0, 2)
  |> clear_rev (0, 3)
  |> flag_rev (1, 0)

let b_remove_flag = b_with_1_flag |> flag_rev (1, 0)

let b_flag_cleared =
  Board.generate_from_bool_grid bb2
  |> clear_rev (0, 1)
  |> clear_rev (0, 2)
  |> clear_rev (0, 3)
  |> flag_rev (0, 1)
  |> flag_rev (0, 2)
  |> flag_rev (0, 3)

let b_clear_flagged =
  Board.generate_from_bool_grid bb2
  |> clear_rev (0, 1)
  |> clear_rev (0, 2)
  |> flag_rev (0, 3)
  |> clear_rev (0, 3)

let b_gen = Board.generate 4 4

let bb_casc1 =
  let z =
    [
      [ 0; 0; 0; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0 ];
      [ 0; 0; 1; 1; 0; 0 ];
      [ 0; 0; 1; 1; 0; 0 ];
      [ 0; 0; 0; 1; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0 ];
    ]
  in
  List.map (List.map (fun a -> a = 1)) z

let bb_casc2 =
  let z =
    [
      [ 0; 0; 0; 1; 0; 0 ];
      [ 0; 0; 1; 0; 0; 0 ];
      [ 0; 1; 0; 0; 0; 0 ];
      [ 1; 0; 0; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 1 ];
    ]
  in
  List.map (List.map (fun a -> a = 1)) z

let bb_casc3 =
  let z =
    [
      [ 1; 1; 1; 1; 1; 1 ];
      [ 1; 0; 0; 0; 0; 1 ];
      [ 1; 0; 0; 0; 0; 1 ];
      [ 1; 0; 0; 0; 0; 1 ];
      [ 1; 0; 0; 0; 1; 1 ];
      [ 1; 1; 1; 1; 1; 1 ];
    ]
  in
  List.map (List.map (fun a -> a = 1)) z

let b_casc1 = Board.generate_from_bool_grid bb_casc1 |> clear_rev (0, 0)

let b_casc2 =
  Board.generate_from_bool_grid bb_casc2
  |> clear_rev (4, 4)
  |> clear_rev (3, 3)
  |> flag_rev (5, 5)

let b_casc3 =
  Board.generate_from_bool_grid bb_casc3
  |> clear_rev (1, 1)
  |> clear_rev (2, 2)
  |> flag_rev (0, 0)

let b_won_vis_board =
  [
    "A  X 2 1 _ ";
    "B  2 X 2 1 ";
    "C  1 2 X 2 ";
    "D  _ 1 2 X ";
    "";
    "   A B C D ";
  ]

let still_playing_vis_board =
  [
    "A  X 2 1 _ ";
    "B  X X 2 1 ";
    "C  X X X X ";
    "D  X X X X ";
    "";
    "   A B C D ";
  ]

let start_vis_board =
  [
    "A  X X X X ";
    "B  X X X X ";
    "C  X X X X ";
    "D  X X X X ";
    "";
    "   A B C D ";
  ]

let b_vis_max_adjacent_bomb =
  [
    "A  X X X X ";
    "B  X 8 X X ";
    "C  X X X X ";
    "D  X X X X ";
    "";
    "   A B C D ";
  ]

let b_vis_1flag =
  [
    "A  X 2 1 _ ";
    "B  ? X 2 1 ";
    "C  X X X X ";
    "D  X X X X ";
    "";
    "   A B C D ";
  ]

let b_vis_remove_flag =
  [
    "A  X 2 1 _ ";
    "B  X X 2 1 ";
    "C  X X X X ";
    "D  X X X X ";
    "";
    "   A B C D ";
  ]

let b_vis_flag_cleared =
  [
    "A  X 2 1 _ ";
    "B  X X 2 1 ";
    "C  X X X X ";
    "D  X X X X ";
    "";
    "   A B C D ";
  ]

let b_vis_clear_flagged =
  [
    "A  X 2 1 _ ";
    "B  X X 2 1 ";
    "C  X X X X ";
    "D  X X X X ";
    "";
    "   A B C D ";
  ]

let b_vis_casc1 =
  [
    "A  _ _ _ _ _ _ ";
    "B  _ 1 2 2 1 _ ";
    "C  _ 2 X X 2 _ ";
    "D  _ 2 X X 3 _ ";
    "E  _ 1 3 X 2 _ ";
    "F  _ _ 1 X 1 _ ";
    "";
    "   A B C D E F ";
  ]

let b_vis_casc2 =
  [
    "A  X X X X 1 _ ";
    "B  X X X 2 1 _ ";
    "C  X X 2 1 _ _ ";
    "D  X 2 1 _ _ _ ";
    "E  1 1 _ _ 1 1 ";
    "F  _ _ _ _ 1 ? ";
    "";
    "   A B C D E F ";
  ]

let b_vis_casc3 =
  [
    "A  ? X X X X X ";
    "B  X 5 3 3 5 X ";
    "C  X 3 _ _ 3 X ";
    "D  X 3 _ 1 4 X ";
    "E  X 5 3 4 X X ";
    "F  X X X X X X ";
    "";
    "   A B C D E F ";
  ]

(*Lists of tests*)
let board_test =
  [
    ( "Boolean board production test" >:: fun _ ->
      assert_equal bg_small_slist
        Board.(generate_from_bool_grid bg_small |> to_string_list) )
    (*Mario Tests*);
    ( "win final board" >:: fun _ ->
      assert_equal b_won_vis_board Board.(b_won |> to_string_list) );
    ( "is_complete true test" >:: fun _ ->
      assert_equal true (b_won |> Board.is_complete) );
    ( "is_complete false test" >:: fun _ ->
      assert_equal false (b_not_complete |> Board.is_complete) );
    ( "is_valid false test" >:: fun _ ->
      assert_equal false (b_not_valid |> Board.is_valid) );
    ( "is_valid true test" >:: fun _ ->
      assert_equal true (b_not_complete |> Board.is_valid) );
    ( "testing board mid game" >:: fun _ ->
      assert_equal still_playing_vis_board
        (b_not_complete |> Board.to_string_list)
        ~printer:pp_lst );
    ( "testing board at start of game" >:: fun _ ->
      assert_equal start_vis_board (b_start |> Board.to_string_list) );
    ( "testing case where 8 bombs surround clear cell" >:: fun _ ->
      assert_equal b_vis_max_adjacent_bomb
        (b_max_adjacent_bombs |> Board.to_string_list) );
    ( "testing 1 flag on board" >:: fun _ ->
      assert_equal b_vis_1flag (b_with_1_flag |> Board.to_string_list) );
    ( "testing remove flag on board" >:: fun _ ->
      assert_equal b_vis_remove_flag
        (b_remove_flag |> Board.to_string_list)
        ~printer:pp_lst );
    ( "testing flag cleared spot " >:: fun _ ->
      assert_equal b_vis_flag_cleared
        (b_flag_cleared |> Board.to_string_list)
        ~printer:pp_lst );
    ( "testing genrate func" >:: fun _ ->
      assert_equal start_vis_board (b_gen |> Board.to_string_list) );
    ( "testing genrated board size" >:: fun _ ->
      assert_equal (4, 4) (b_gen |> Board.dimensions) );
    ( "testing genrated board that it is not complete" >:: fun _ ->
      assert_equal false (b_gen |> Board.is_complete) );
    ( "testing genrated board that it is valid" >:: fun _ ->
      assert_equal true (b_gen |> Board.is_valid) );
    ( "testing clear flagged spot " >:: fun _ ->
      assert_equal b_vis_clear_flagged
        (b_clear_flagged |> Board.to_string_list)
        ~printer:pp_lst );
    ( "testing cascade around outside " >:: fun _ ->
      assert_equal b_vis_casc1 (b_casc1 |> Board.to_string_list) ~printer:pp_lst
    );
    ( "testing cascade around outside " >:: fun _ ->
      assert_equal b_vis_casc2 (b_casc2 |> Board.to_string_list) ~printer:pp_lst
    );
    ( "testing cascade when surrounded by bombds " >:: fun _ ->
      assert_equal b_vis_casc3 (b_casc3 |> Board.to_string_list) ~printer:pp_lst
    );
  ]

(*Cell pretty print*)
let pp_cellValue (c : cell) : string = Cell.to_int c |> string_of_int
let pp_charValue (c : char) : string = String.make 1 c

(*Cell generate helper function*)
let generate_test (name : string) (input : int) (expected_output : cell) =
  name >:: fun _ ->
  assert_equal expected_output (generate input) ~printer:pp_cellValue

let to_int_test (name : string) (input : cell) (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (to_int input) ~printer:string_of_int

let to_char_test (name : string) (input : cell) (expected_output : char) =
  name >:: fun _ ->
  assert_equal expected_output (to_char input) ~printer:pp_charValue

let cell_test =
  [
    generate_test "Generate Mine cell" (-1) mineCell;
    generate_test "Generate Empty cell" 0 emptyCell;
    generate_test "Generate value 1 cell" 1 v1Cell;
    generate_test "Generate value 2 cell" 2 v2Cell;
    generate_test "Generate value 3 cell" 3 v3Cell;
    generate_test "Generate value 4 cell" 4 v4Cell;
    generate_test "Generate value 5 cell" 5 v5Cell;
    generate_test "Generate value 6 cell" 6 v6Cell;
    generate_test "Generate value 7 cell" 7 v7Cell;
    generate_test "Generate value 8 cell" 8 v8Cell;
    to_int_test "int of value 1 cell" v1Cell 1;
    to_int_test "int of value 2 cell" v2Cell 2;
    to_int_test "int of value 3 cell" v3Cell 3;
    to_int_test "int of value 4 cell" v4Cell 4;
    to_int_test "int of value 5 cell" v5Cell 5;
    to_int_test "int of value 6 cell" v6Cell 6;
    to_int_test "int of value 7 cell" v7Cell 7;
    to_int_test "int of value 8 cell" v8Cell 8;
    to_int_test "int of Mine Cell" mineCell (-1);
    to_int_test "int of empty Cell" emptyCell 0;
    to_char_test "Mine Cell" mineCell 'X';
    (*Mario tests*)
    to_char_test "Non Mine" v1Cell 'X';
    to_char_test "emptycell to_char" emptyCell 'X';
    to_char_test "cleared empty cell to char" (clear emptyCell) '_';
    to_char_test "cleared non_mine cell to char" (clear v1Cell) '1';
    to_char_test "cleared mine to char" (clear mineCell) '!';
    to_char_test "flagged cell " (flag v1Cell) '?';
    to_char_test "cleared non_mine cell to char" (clear v3Cell) '3';
    ( "non integer args clear" >:: fun _ ->
      assert_raises Cell.IntegerInputOutOfRange (fun () -> generate 9) );
  ]

let parse_tester (name : string) (input : string)
    (expected_output : Command.command) =
  name >:: fun _ -> assert_equal expected_output (Command.parse input)

let command_test =
  [
    parse_tester "clear valid" "clear 1 1" (Command.Clear (1, 1));
    parse_tester "flag valid" "flag 0 0" (Command.Flag (0, 0));
    parse_tester "flag valid extra space " "flag 0  0" (Command.Flag (0, 0));
    parse_tester "quit tester" "quit" Command.Quit;
    parse_tester "quit tester" "restart" Command.Restart;
    ( "testing invalid quit command" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "quit 0") );
    ( "testing invalid restart command" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "restart 0") );
    ( "invalid clear with one arg" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "clear 0") );
    ( "invalid clear with more than 2 arg" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "clear 0 0 0") );
    ( "clear with 0 args" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "clear") );
    ( "invalid flag with one arg" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "flag 0") );
    ( "invalid flag with more than 2 arg" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "flag 0 0 0") );
    ( "flag with 0 args" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "flag") );
    ( "3 random strings" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "0 0 0") );
    ( "empty command" >:: fun _ ->
      assert_raises Command.Empty (fun () -> Command.parse "") );
    (* ( "non integer args flag" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "flag j j") );
    ( "non integer args clear" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "clear j j") ); *)
  ]

(*extracts the state from Result type output of State.clear*)
let get_clear str st =
  match State.clear str st with
  | Legal sta -> sta
  | Illegal -> failwith "Fix the invalid input in test file"

let get_flag str st =
  match State.flag str st with
  | Legal sta -> sta
  | Illegal -> failwith "Fix the invalid input in test file"

let s2 =
  State.initial_state_test (Board.generate_from_bool_grid bb2)
  |> get_clear "0 1" |> get_clear "0 2" |> get_clear "0 3" |> get_clear "1 0"
  |> get_clear "1 2" |> get_clear "1 3" |> get_clear "2 0" |> get_clear "2 1"
  |> get_clear "2 3" |> get_clear "3 0" |> get_clear "3 1" |> get_clear "3 2"

let s3 =
  State.initial_state_test (Board.generate_from_bool_grid bb2)
  |> get_clear "0 1" |> get_clear "0 2" |> get_clear "0 0"

let s4 =
  State.initial_state_test (Board.generate_from_bool_grid bb2)
  |> get_clear "0 1" |> get_clear "0 2" |> get_clear "1 3"

let s5 = s4 |> get_flag "0 0"

let state_test =
  [
    ("win state" >:: fun _ -> assert_equal State.Won (State.is_game_over s2));
    ("lose state" >:: fun _ -> assert_equal State.Lost (State.is_game_over s3));
    ("lose state" >:: fun _ -> assert_equal State.Play (State.is_game_over s4));
    ( "flag on bomb is valid" >:: fun _ ->
      assert_equal State.Play (State.is_game_over s5) );
    ( "test get_board" >:: fun _ ->
      assert_equal b_won_vis_board
        (State.get_current_board s2 |> Board.to_string_list)
        ~printer:pp_lst );
  ]

let suite =
  "Test Suite for Board Cell Command and State"
  >::: List.flatten [ board_test; cell_test; command_test; state_test ]

let _ = run_test_tt_main suite
