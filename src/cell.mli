(** Representation of a single cell and its current state. *)

(*** Types ********************************************************************)

type cell

exception MineUncovered
(** Rased when Cleared Cell contains a mine*)

exception IntegerInputOutOfRange
(** Used by generate to prevent invalid cells from being created*)

exception ReclearAttempted
(** Used in cascading to determine whether an empty cell has already been
    cleared to prevent imfinite looping*)

(*** Functions ****************************************************************)

val generate : int -> cell
(** [generate i] is a cell [c] with [Hidden] visibility and a type dictated by
    [i]: | i = -1 -> Mine | i = 0 -> Empty | i > 0 -> [i] mines adjacent. *)

val clear : cell -> cell
(** [clear c] is the cell [c] after the player has attempted to clear it. *)

val clear_volatile : cell -> cell
(** [clear_volatile c] is the cell [c] after the player has attempted to clear
    it and throws exception if you have a mineuncovered *)

val clear_volatile_cascade : bool ref -> cell -> cell
(** [clear_volatile cascde r c] is the cell [c] after the player has attempted
    to clear it and throws an exception if you have a mine uncoverred like in
    clear volatile. Modifies the ref [r] to be true if the cell uncoverred has
    no adjacent mines *)

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
