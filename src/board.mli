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

val int_grid_to_bool_grid : int list list -> bool list list
(** [int_grid_to_bool_grid int_grid] is a boolean grid corresponding to the 1s
    and 0s in [int_grid]. *)

val dimensions : board -> int * int
(** [dimensions brd] is a tuple [(m*n)] corresponding to [m] columns and [n]
    rows. *)

val is_valid : board -> bool
(** [is_valid brd] is a boolean declaring whether a board is valid (if the
    player has not attempted to clear any mined cells). *)

val is_complete : board -> bool
(** [is_complete brd] is a boolean declaring whether a board is complete (all
    non-mined cells have been cleared) *)

val mines_left : board -> int
(** [mines_left brd] is an int denoting the number of mines which have not yet
    been uncovered*)

val uncover_board : board -> board
(**[uncover_board brd] is the board [brd] with the value of all cells set to
   visible and a failed gamestate*)
