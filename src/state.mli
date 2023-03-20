type t
(** The abstract type of values representing the game state. *)

val init_state : int -> int -> t
(** [init_state] is the initial state of the game when playing with a moard of m
    x n. In that state the adventurer is currently located in the starting room,
    and they have visited only that room. *)

val get_current_board : t -> Board.board
(** [current_board st] returns the board associated with the current state*)

val clicked : t -> (int * int) list
(** [visited st] is a set-like list of the cell identifiers the player has
    clicked. *)

val is_game_over : t -> bool
(** [is_game_over st] returns true if the game state signifies that the game is
    over *)

(** The type representing the result of a clear *)
type result =
  | Legal of t
  | Illegal

val clear : string -> t -> result
(** [go input st] is the result of attempting to a position [input] in state
    [st]*)