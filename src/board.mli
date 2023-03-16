(** Representation of static adventure data.

    This module represents the data stored in adventure files, including the
    rooms and exits. It handles loading of that data from JSON as well as
    querying the data.

    For examples, the specifications in this interface reference the example "Ho
    Plaza" adventure found in [data/ho_plaza.json]. *)

(*** Types ********************************************************************)

type brd
(** The abstract type of values representing a board and game state. *)

type position
(** The abstract type of values representing a location on an abstract board. *)

(*** Functions ****************************************************************)

val clear_position : brd -> position -> brd
(** [clear_position x y] is the board [x] with position [y] having been
    attempted to be cleared, regardless of the success of the clearing. *)

val to_string_list : brd -> string list
(** [to_string_list x] is a list of strings which when printed represent the
    board [x]. *)

val display : brd -> _
(** [display x] prints a representation of the board [x] to the command line. *)

val generate : int -> int -> brd
(** [generate m n] is a board with dimensions [m] columns and [n] rows with no
    tiles revealed. *)
