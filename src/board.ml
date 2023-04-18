type board = {
  validity : bool;
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

let generate_grid_naive controller (m : int) (n : int) =
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
      if i > w then "" else row_of_int i ^ labels_of_width_helper (i + 1)
    in
    labels_of_width_helper 0
  in
  (*Helper function 2*)
  let rec string_of_brdGrd_helper (ind : int) (grid : Cell.cell list list) :
      string list =
    match grid with
    | [] -> []
    | [ h ] ->
        (row_of_int ind ^ " " ^ string_of_row h)
        :: ""
        :: [ "   " ^ labels_of_width (brd.n - 1) ]
    | h :: t ->
        (row_of_int ind ^ " " ^ string_of_row h)
        :: string_of_brdGrd_helper (ind + 1) t
  in
  string_of_brdGrd_helper 0 brd.grid

let parse_boolean_lists above at below =
  (*Internal helper 1*)
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
  try { brd with grid = mod_indof_grid Cell.clear_volatile position brd.grid }
  with Cell.MineUncovered ->
    {
      brd with
      grid = mod_indof_grid Cell.clear position brd.grid;
      validity = false;
    }

let flag_position brd (position : int * int) =
  { brd with grid = mod_indof_grid Cell.flag position brd.grid }

let to_string_list brd : string list = string_of_board brd

let generate m n =
  {
    validity = true;
    grid =
      generate_grid_naive
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
