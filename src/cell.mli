(** Representation of a single cell and its current state. *)

(*** Types ********************************************************************)

type cell

(*** Functions ****************************************************************)

val generate : int -> cell
(** [generate i] is a cell [c] with [Hidden] visibility and a type dictated by
    [i]: | i = -1 -> Mine | i = 0 -> Empty | i > 0 -> [i] mines adjacent. *)

val clear : cell -> cell
(** [clear c] is the cell [c] after the player has attempted to clear it. *)

val flag : cell -> cell
(** [flag c] is the cell [c] after the player has attempted to flag it. *)

val to_char : cell -> char
(** [to_char c] is the character representing the cell [c] in its current
    visibility state. *)

val to_int : cell -> int
(** [to_int c] is the integer representing the cell [c] in its current state.
    This function is the inverse of [generate]*)

val not_mine : cell -> bool
(** [not_mine c] is a boolean representing whether the cell [c] represents a
    mine. *)
