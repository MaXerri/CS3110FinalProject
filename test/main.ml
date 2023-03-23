open OUnit2
open Minesweeper
open Board
open Cell
open Command
open State

(*let cmp_cells (cell1 : cell) (cell2 : cell) = true*)


(*Cell generate helper function*)
let generate_test (name : string) (input : int) (expected_output: cell) = 
  name >:: fun _ ->
    assert_equal expected_output (generate input)





let board_test = []

let cell_test = []

let command_test = []

let state_test = []

let suite = 
  "Test Suite for Board Cell Command and State"
  >::: List.flatten [board_test; cell_test; command_test; state_test]

let _ = run_test_tt_main suite