type board = {
  grid : Cell.cell list list;
  m : int;
  n : int;
}
(** The abstract type of values representing a board and game state. *)

type position
(** The abstract type of values representing a location on an abstract board. *)

(*** Functions ****************************************************************)

let clear_position = raise (Failure "Unimplemented: Board.clear_position")
let to_string_list = raise (Failure "Unimplemented: Board.to_string_list")
let display = raise (Failure "Unimplemented: Board.display")
let generate = raise (Failure "Unimplemented: Board.generate")
let grid b = b.grid
