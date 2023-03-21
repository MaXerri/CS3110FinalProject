type board = {
  grid : Cell.cell list list;
  m : int;
  n : int;
}
(** The abstract type of values representing a board and game state. *)

(*** Functions ****************************************************************)

(**[mod_indof_list foi i lst] returns [lst] with the [i]th element of [lst]
   modified by [foi]*)
let rec mod_indof_list foi (ind : int) lst =
  match lst with
  | [] -> raise (Failure "Index out of bounds")
  | h :: t ->
      if ind = 0 then foi h :: t else h :: mod_indof_list foi (ind - 1) t

(**[mod_indof_grid foi (m,n) grd] returns [grd] with the [n]th element of the
   [m]th element of [grd] modified by [foi]*)
let mod_indof_grid foi (ind : int * int) (grd : 'a list list) =
  match ind with
  | m, n -> mod_indof_list (mod_indof_list foi n) m grd

let clear_position brd (tup : int * int) =
  { brd with grid = mod_indof_grid Cell.clear tup brd.grid }

let to_string_list = raise (Failure "Unimplemented: Board.to_string_list")
let display = raise (Failure "Unimplemented: Board.display")
let generate = raise (Failure "Unimplemented: Board.generate")
let dimensions brd = (brd.m, brd.n)
