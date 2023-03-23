open OUnit2
open Minesweeper
open Board
open Cell
open Command
open State

(*let cmp_cells (cell1 : cell) (cell2 : cell) = true*)

(*temporary*)
let mineCell = generate (-1);;
let emptyCell = generate (0);;
let v1Cell = generate (1);;



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
  generate_test "Generate value 1 cell" (1) v1Cell
]

let command_test = []

let state_test = []

let suite = 
  "Test Suite for Board Cell Command and State"
  >::: List.flatten [board_test; cell_test; command_test; state_test]

let _ = run_test_tt_main suite