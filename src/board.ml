type board = {
  grid : Cell.cell list list;
  m : int;
  n : int;
}
(** The abstract type of values representing a board and game state. *)

(*** Helper Functions *********************************************************)

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

(***)
let rec generate_list controller (l : int) =
  if l = 0 then [] else controller true :: generate_list controller (l - 1)

let generate_grid controller (m : int) (n : int) =
  generate_list (fun a -> if a then generate_list controller n else []) m

(**Returns string corresponding to character [c]*)
let s_of_c c = String.make 1 c

(**Returns the string representation of the row [row]*)
let rec string_of_row (row : Cell.cell list) : string =
  match row with
  | [] -> ""
  | h :: t -> s_of_c (Cell.to_char h) ^ " " ^ string_of_row t

(**Returns row header for given index*)
let row_of_int i =
  if i < 27 then s_of_c (Char.chr (i + 65)) ^ " "
  else raise (Failure "integer out of numerical index")

(**Returns a list of strings representing the rows of a game board which contain
   cells*)
let string_of_board brd =
  (*Helper function 1*)
  let labels_of_width w =
    let rec labels_of_width_helper i : string =
      if i > w then "" else row_of_int (65 + i) ^ labels_of_width_helper (i + 1)
    in
    labels_of_width_helper w
  in
  (*Helper function 2*)
  let rec string_of_brdGrd_helper (ind : int) (grid : Cell.cell list list) :
      string list =
    match grid with
    | [] -> "" :: [ "  " ^ labels_of_width brd.n ]
    | [ h ] -> [ row_of_int ind ^ string_of_row h ]
    | h :: t ->
        (row_of_int ind ^ string_of_row h)
        :: string_of_brdGrd_helper (ind + 1) t
  in
  string_of_brdGrd_helper 0 brd.grid

(*** Functions ****************************************************************)

let clear_position brd (tup : int * int) =
  { brd with grid = mod_indof_grid Cell.clear tup brd.grid }

let flag_position brd (tup : int * int) =
  { brd with grid = mod_indof_grid Cell.flag tup brd.grid }

let to_string_list brd : string list = string_of_board brd

let generate m n =
  {
    grid =
      generate_grid
        (fun a ->
          if a then Cell.generate 0
          else raise (Failure "Poorly defined controller"))
        m n;
    m;
    n;
  }

let dimensions brd = (brd.m, brd.n)
