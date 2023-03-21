(** Representation of a board and its current state. *)

(*** Types ********************************************************************)

type board
(** The abstract type of values representing a board and game state. *)

(*** Functions ****************************************************************)

val clear_position : board -> int * int -> board
(** [clear_position x y] is the board [x] with position [y] having been
    attempted to be cleared, regardless of the success of the clearing. *)

val flag_position : board -> int * int -> board
(** [flag_position x y] is the board [x] with position [y] having been attempted
    to be flagged. *)

val to_string_list : board -> string list
(** [to_string_list x] is a list of strings which when printed represent the
    board [x]. *)

val generate : int -> int -> board
(** [generate m n] is a board with dimensions [m] columns and [n] rows with no
    tiles revealed. *)

val generate_from_bool_grid : bool list list -> board
(** [generate_from_bool_grid grd] is a board with mines at locations
    corresponding to true values in [grd]. *)

val dimensions : board -> int * int
(** [dimensions brd] is a tuple [(m*n)] corresponding to [m] columns and [n]
    rows. *)
