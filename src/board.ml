type board = {
  validity : bool;
  grid : Cell.cell list list;
  m : int;
  n : int;
}
(** The abstract type of values representing a board and game state. *)

(*** Helper Functions *********************************************************)

(**[modify_list_index foi i lst] returns list [lst] with the [i]th element of
   [lst] modified by function [foi]. Helper function used by
   [modify_grid_index foi].*)
let rec modify_list_index foi (ind : int) lst =
  match lst with
  | [] -> raise (Failure "Index out of bounds")
  | h :: t ->
      if ind = 0 then foi h :: t else h :: modify_list_index foi (ind - 1) t

(**[modify_grid_index foi (m,n) grd] returns grid [grd] with the [n]th element
   of the [m]th element of [grd] modified by function [foi]. Used for
   modification of single elements within grids.*)
let modify_grid_index foi (ind : int * int) (grd : 'a list list) =
  match ind with
  | m, n -> modify_list_index (modify_list_index foi n) m grd

(**[generate_naive_list controller l] generates a list of length [l] with
   elements produced by the function [controller], which generates a value when
   passed a boolean. Useful in the generation of grids for play.*)
let rec generate_naive_list controller (l : int) =
  if l = 0 then [] else controller true :: generate_naive_list controller (l - 1)

(**[generate_naive_grid controller m n] generates a grid of length [m] with
   lists of length [n] , and elements produced by the function [controller],
   which generates a value when passed a boolean. Useful in the generation of
   grids for play.*)
let generate_naive_grid controller (m : int) (n : int) =
  generate_naive_list
    (fun a -> if a then generate_naive_list controller n else [])
    m

(**[char_to_string c] is the string corresponding to character [c]*)
let char_to_string c = String.make 1 c

(**[string_of_row row] is the string representation of a cell grid row [row]*)
let rec string_of_row (row : Cell.cell list) : string =
  match row with
  | [] -> ""
  | h :: t -> char_to_string (Cell.to_char h) ^ " " ^ string_of_row t

(**[row_of_int i] is the row header for given index [i], as designated for a
   gameboard. Example: row 0 has header A, row 1 has header B*)
let row_of_int i =
  if i < 27 then char_to_string (Char.chr (i + 65)) ^ " "
  else raise (Failure "integer out of numerical index")

(**[board_to_stringlist brd] is a string list representing the rows of a game
   board [brd]*)
let board_to_stringlist brd =
  (*[labels_of_width w] generates a string of row labels of length [w]*)
  let labels_of_width w =
    let rec labels_of_width_helper i : string =
      if i > w then "" else row_of_int i ^ labels_of_width_helper (i + 1)
    in
    labels_of_width_helper 0
  in
  (*Recursive function which handles primary functions of
    [board_to_stringlist]*)
  let rec rec_board_to_stringlist (ind : int) (grid : Cell.cell list list) :
      string list =
    match grid with
    | [] -> []
    | [ h ] ->
        (row_of_int ind ^ " " ^ string_of_row h)
        :: ""
        :: [ "   " ^ labels_of_width (brd.n - 1) ]
    | h :: t ->
        (row_of_int ind ^ " " ^ string_of_row h)
        :: rec_board_to_stringlist (ind + 1) t
  in
  rec_board_to_stringlist 0 brd.grid

(**[parse_boolean_lists above at below] is a Cell.cell list with appropriately
   labeled cells (int, mine, etc) representative of the list [at]. Used as a
   helper function by [parse_boolean_grid].*)
let parse_boolean_lists above at below =
  (*Helper function converts boolean lists into integer lists*)
  let bool_list_to_int b_list =
    let bool_inc b i = if b then i + 1 else i in
    let rec b_list_parse blst =
      match blst with
      | [] -> 0
      | h :: t -> bool_inc h (b_list_parse t)
    in
    b_list_parse b_list
  in
  (*Internal helper 2*)
  let rec pbl_helper (above : bool list) (at : bool list) (below : bool list) =
    match (above, at, below) with
    | [ h0; h1 ], [ i0; i1 ], [ j0; j1 ] ->
        let c =
          if i1 then Cell.generate (-1)
          else Cell.generate (bool_list_to_int [ h0; h1; i0; j0; j1 ])
        in
        [ c ]
    | h0 :: h1 :: h2 :: t_ab, i0 :: i1 :: i2 :: t_at, j0 :: j1 :: j2 :: t_bl ->
        let c =
          if i1 then Cell.generate (-1)
          else
            Cell.generate (bool_list_to_int [ h0; h1; h2; i0; i2; j0; j1; j2 ])
        in
        c :: pbl_helper (h1 :: h2 :: t_ab) (i1 :: i2 :: t_at) (j1 :: j2 :: t_bl)
    | _, _, _ ->
        raise
          (Failure
             {|Improper parsing in Board.parse_boolean_lists "Internal helper 2"|})
  in
  (*Execution*)
  pbl_helper above at below

(**[gen_false_list len] is a boolean list of [false] values of length [len]*)
let rec gen_false_list (len : int) =
  if len < 0 then [] else false :: gen_false_list (len - 1)

let parse_boolean_grid (grd_in : bool list list) =
  let cols =
    match grd_in with
    | h :: _ -> List.length h
    | [] -> 0
  in
  let f_list = gen_false_list cols in
  let grd_app = f_list :: grd_in in
  let rec parse_grid grd : Cell.cell list list =
    match grd with
    | r0 :: r1 :: r2 :: t ->
        parse_boolean_lists (false :: r0) (false :: r1) (false :: r2)
        :: parse_grid (r1 :: r2 :: t)
    | [ r0; r1 ] -> [ parse_boolean_lists r0 r1 f_list ]
    | _ -> raise (Failure "Unnaccounted for failure in parse_boolean_grid")
  in
  parse_grid grd_app

(*** Functions ****************************************************************)

let clear_position brd (position : int * int) =
  try
    { brd with grid = modify_grid_index Cell.clear_volatile position brd.grid }
  with Cell.MineUncovered ->
    {
      brd with
      grid = modify_grid_index Cell.clear position brd.grid;
      validity = false;
    }

let flag_position brd (position : int * int) =
  { brd with grid = modify_grid_index Cell.flag position brd.grid }

let to_string_list brd : string list = board_to_stringlist brd

let generate m n =
  {
    validity = true;
    grid =
      generate_naive_grid
        (fun a ->
          if a then Cell.generate 0
          else raise (Failure "Poorly defined controller"))
        m n;
    m;
    n;
  }

let generate_from_bool_grid bool_grd =
  {
    validity = true;
    grid = parse_boolean_grid bool_grd;
    m = List.length bool_grd;
    n =
      (match bool_grd with
      | h :: _ -> List.length h
      | _ -> raise (Failure "Invalid grid input (empty rows)"));
  }

let dimensions brd = (brd.m, brd.n)
let is_valid brd = brd.validity

(*this is a testing comment to check branches*)
