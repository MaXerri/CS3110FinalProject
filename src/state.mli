(**Representation of the current state of the game*)

(*** Types ********************************************************************)
type t
(** The abstract type of values representing the game state. *)

(**Type representing if the game is still playing, has been won or has been lost*)
type g_over =
  | Won
  | Lost
  | Play

val init_state : int -> int -> t
(** [init_state m n] is the initial state of the game when playing with a moard
    of [m] x [n]. In that state the adventurer is currentlystarting with a board
    that has no cells cleared yet *)

val initial_state_test : Board.board -> t
(** [init_state b] is the initial state of the game when playing with a starting
    board [b]. This will be used for testing so we can input bards to create
    custom starting states. *)

val get_current_board : t -> Board.board
(** [current_board st] returns the board associated with the current state [st]*)

val is_game_over : t -> g_over
(** [is_game_over st] returns true if the game state [st] signifies that the
    game is over *)

(** The type representing the result of a clear *)
type result =
  | Legal of t
  | Illegal

val clear : string -> t -> result
(** [go input st] is the result of attempting to clear a position [input] in
    state [st]*)

val flag : string -> t -> result
(** [go input st] is the result of attempting to flag a position [input] in
    state [st]*)
