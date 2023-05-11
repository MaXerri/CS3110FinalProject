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

(*Lists of tests*)
let board_test =
  [
    ( "Boolean board" >:: fun _ ->
      assert_equal bg_small_slist
        Board.(generate_from_bool_grid bg_small |> to_string_list) );
  ]

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
  ]

let command_test = []
let state_test = []

let suite =
  "Test Suite for Board Cell Command and State"
  >::: List.flatten [ board_test; cell_test; command_test; state_test ]

let _ = run_test_tt_main suite
