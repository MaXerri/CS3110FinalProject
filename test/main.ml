(*Test plan: *)
(**Everything tested in *)

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
let badV9Cell = generate 9

(*let badCell = generate (-2)*)
(*let maxIntCell = generate 1073741823*)
(*let minIntCell = generate (-1073741824)*)

(*Boolean Board testing*)
let bg_small = [ [ true; false ]; [ false; false ] ]
let bg_small_slist = [ "A  X X"; "B  X X"; ""; "   A B" ]

(*switches the arguments of Board.clear_position to allow for pipelining*)
let clear_rev t b = Board.clear_position b t

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

let b_won_vis_board =
  [ "A  X _ _ _"; "B  _ X _ _"; "C  _ _ X _"; "D  _ _ _ X"; "   A B C D" ]

let still_playing_vis_board =
  [ "A  X 2 1 _"; "B  _ X 2 X"; "C  X X X X"; "D  X X X X"; "   A B C D" ]

let start_vis_board =
  [ "A  X X X X"; "B  X X X X"; "C  X X X X"; "D  X X X X"; "   A B C D" ]

let b_vis_max_adjacent_bomb =
  [ "A  X X X X"; "B  X 8 X X"; "C  X X X X"; "D  X X X X"; "   A B C D" ]

(*Lists of tests*)
let board_test =
  [
    ( "Boolean board" >:: fun _ ->
      assert_equal bg_small_slist
        Board.(generate_from_bool_grid bg_small |> to_string_list) )
    (*Mario Tests*);
    ( "sequential test" >:: fun _ ->
      assert_equal bg_small_slist
        Board.(generate_from_bool_grid bg_small |> to_string_list) );
    ( "win final board" >:: fun _ ->
      assert_equal b_won_vis_board Board.(b_won |> to_string_list) );
    ( "is_complete true test" >:: fun _ ->
      assert_equal true (b_won |> Board.is_complete) );
    ( "is_complete false test" >:: fun _ ->
      assert_equal false (b_not_complete |> Board.is_complete) );
    ( "is_valid false test" >:: fun _ ->
      assert_equal false (b_not_valid |> Board.is_complete) );
    ( "is_valid true test" >:: fun _ ->
      assert_equal true (b_not_complete |> Board.is_complete) );
    ( "testing board mid game" >:: fun _ ->
      assert_equal still_playing_vis_board
        (b_not_complete |> Board.to_string_list) );
    ( "testing board at start of game" >:: fun _ ->
      assert_equal start_vis_board (b_start |> Board.to_string_list) );
    ( "testing case where 8 bombs surround clear cell" >:: fun _ ->
      assert_equal b_vis_max_adjacent_bomb
        (b_max_adjacent_bombs |> Board.to_string_list) );
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
    generate_test "Generate value 9 cell" 9 badV9Cell;
    (*generate_test "Generate value -2 cell" (-2) badCell;*)
    (* generate_test "Generate max int cell" 1073741823 maxIntCell;*)
    (*generate_test "Generate max int cell" (-1073741824) minIntCell;*)
    to_int_test "int of value 1 cell" v1Cell 1;
    to_int_test "int of value 2 cell" v2Cell 2;
    to_int_test "int of value 3 cell" v3Cell 3;
    to_int_test "int of value 4 cell" v4Cell 4;
    to_int_test "int of value 5 cell" v5Cell 5;
    to_int_test "int of value 6 cell" v6Cell 6;
    to_int_test "int of value 7 cell" v7Cell 7;
    to_int_test "int of value 8 cell" v8Cell 8;
    to_int_test "int of value 9 cell" badV9Cell 9;
    to_int_test "int of Mine Cell" mineCell (-1);
    to_int_test "int of empty Cell" emptyCell 0;
    (*to_int_test "int of value -2 Cell" badCell (-2);*)
    (* to_int_test "int of max int cell" maxIntCell 1073741823;*)
    (* to_int_test "int of min int cell" minIntCell (-1073741824);*)
    to_char_test "Mine Cell" mineCell 'X';
    (*Mario tests*)
    to_char_test "Non Mine" v1Cell 'X';
    to_char_test "emptycell to_char" emptyCell 'X';
    to_char_test "cleared empty cell to char" (clear emptyCell) '_';
    to_char_test "cleared non_mine cell to char" (clear v1Cell) '1';
    to_char_test "cleared mine to char" (clear mineCell) '!';
    to_char_test "flagged cell " (flag v1Cell) '?';
    to_char_test "cleared non_mine cell to char" (clear v3Cell) '3';
  ]

let parse_tester (name : string) (input : string)
    (expected_output : Command.command) =
  name >:: fun _ -> assert_equal expected_output (Command.parse input)

let command_test =
  [
    parse_tester "clear valid" "clear 1 1" (Command.Clear (1, 1));
    parse_tester "flag valid" "clear 0 0" (Command.Flag (0, 0));
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
      assert_raises Command.Empty (fun () -> Command.parse "clear") );
    ( "invalid flag with one arg" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "flag 0") );
    ( "invalid flag with more than 2 arg" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "flag 0 0 0") );
    ( "flag with 0 args" >:: fun _ ->
      assert_raises Command.Empty (fun () -> Command.parse "flag") );
    ( "3 random chars" >:: fun _ ->
      assert_raises Command.Malformed (fun () -> Command.parse "0 0 0") )
    (*May be handled in main *);
  ]

(*extracts the state from Result type output of State.clear*)
let get_clear str st =
  match State.clear str st with
  | Legal st -> st
  | Illegal -> failwith "Fix the invalid input in test file"

let s2 =
  State.initial_state_test (Board.generate_from_bool_grid bb2)
  |> get_clear "clear 0 1" |> get_clear "clear 0 2" |> get_clear "clear 0 3"
  |> get_clear "clear 1 0" |> get_clear "clear 1 2" |> get_clear "clear 1 3"
  |> get_clear "clear 2 0" |> get_clear "clear 2 1" |> get_clear "clear 2 3"
  |> get_clear "clear 3 0" |> get_clear "clear 3 1" |> get_clear "clear 3 2"

let s3 =
  State.initial_state_test (Board.generate_from_bool_grid bb2)
  |> get_clear "clear 0 1" |> get_clear "clear 0 2" |> get_clear "clear 0 0"

let s4 =
  State.initial_state_test (Board.generate_from_bool_grid bb2)
  |> get_clear "clear 0 1" |> get_clear "clear 0 2" |> get_clear "clear 1 3"

let state_test =
  [
    ("win state" >:: fun _ -> assert_equal State.Won (State.is_game_over s2));
    ("lose state" >:: fun _ -> assert_equal State.Lost (State.is_game_over s3));
    ("lose state" >:: fun _ -> assert_equal State.Play (State.is_game_over s4));
    ( "test get_board" >:: fun _ ->
      assert_equal b_won_vis_board
        (State.get_current_board s2 |> Board.to_string_list) );
  ]

let suite =
  "Test Suite for Board Cell Command and State"
  >::: List.flatten [ board_test; cell_test; command_test; state_test ]

let _ = run_test_tt_main suite
