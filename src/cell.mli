(** Representation of a single cell and its current state. *)

(*** Types ********************************************************************)

type cell

(*** Functions ****************************************************************)

val clear : cell -> cell
(** [clear c] is the cell [c] after the player has attempted to clear it. *)

val to_char : cell -> char
(** [to_char c] is the character representing the cell [c] in its current
    visibility state. *)
