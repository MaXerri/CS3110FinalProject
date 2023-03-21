type board = {
  grid : Cell.cell list list;
  m : int;
  n : int;
}
(** The abstract type of values representing a board and game state. *)

(*** Functions ****************************************************************)

let clear_position (brd : board) (tup : int * int) : board =
  raise (Failure "Unimplemented: Board.clear_position")

let to_string_list = raise (Failure "Unimplemented: Board.to_string_list")
let display = raise (Failure "Unimplemented: Board.display")
let generate = raise (Failure "Unimplemented: Board.generate")
let dimensions brd = (brd.m, brd.n)
