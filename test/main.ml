open OUnit2
open Minesweeper
open Board
open Cell
open Command
open State

(*let cmp_cells (cell1 : cell) (cell2 : cell) = true*)

(*temporary cells for testing purposes*)
let mineCell = generate (-1);;
let emptyCell = generate (0);;
let v1Cell = generate (1);;
let v2Cell = generate (2);;
let v3Cell = generate (3);;
let v4Cell = generate (4);;
let v5Cell = generate (5);;
let v6Cell = generate (6);;
let v7Cell = generate (7);;
let v8Cell = generate (8);;
let badCell = generate (-2);;



(*Cell pretty print*)
let pp_cellValue (c : cell) : string = Cell.to_int c |> string_of_int

(*Cell generate helper function*)
let generate_test (name : string) (input : int) (expected_output: cell) = 
  name >:: fun _ ->
    assert_equal expected_output (generate input) ~printer: pp_cellValue




(*Lists of tests*)
let board_test = []

let cell_test = [
  generate_test "Generate Mine cell" (-1) mineCell;
  generate_test "Generate Empty cell" (0) emptyCell;
  generate_test "Generate value 1 cell" (1) v1Cell;
  generate_test "Generate value 1 cell" (2) v2Cell;
  generate_test "Generate value 1 cell" (3) v3Cell;
  generate_test "Generate value 1 cell" (4) v4Cell;
  generate_test "Generate value 1 cell" (5) v5Cell;
  generate_test "Generate value 1 cell" (6) v6Cell;
  generate_test "Generate value 1 cell" (7) v7Cell;
  generate_test "Generate value 1 cell" (8) v8Cell;
  generate_test "Generate value 1 cell" (-2) badCell

]

let command_test = []

let state_test = []

let suite = 
  "Test Suite for Board Cell Command and State"
  >::: List.flatten [board_test; cell_test; command_test; state_test]

let _ = run_test_tt_main suite