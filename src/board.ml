type board = {
  validity : bool;
  remainingCells : int;
  grid : Cell.cell list list;
  m : int;
  n : int;
}
(** The abstract type of values representing a board and game state. *)

(*** Helper Functions *********************************************************)

(**[modify_list_index foi i lst] returns list [lst] with the [i]th element of
   [lst] modified by function [foi]. Helper function used by
   [modify_grid_index foi].*)
let rec modify_list_index (foi : 'a -> 'a) (ind : int) lst : 'a list =
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

(**[mutate_grid foi grd] returns grid [grd] with each modified by function
   [foi]. Used for modification of all elements within a grid.*)
let mutate_grid (foi : 'a -> 'a) (grd : 'a list list) : 'a list list =
  let rec mutate_list f ls =
    match ls with
    | h :: t -> f h :: mutate_list f t
    | [] -> []
  in
  mutate_list (mutate_list foi) grd

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
  (*Helper function counts the number of true values in a boolean list*)
  let bool_list_to_int b_list =
    let bool_inc b i = if b then i + 1 else i in
    (*Recursively counts booleans using [bool_list_to_int]*)
    let rec b_list_parse blst =
      match blst with
      | [] -> 0
      | h :: t -> bool_inc h (b_list_parse t)
    in
    b_list_parse b_list
  in
  (*Recursive helper which handles functionality; Returns a bool list two units
    shorter than its input*)
  let rec pbl_helper (above : bool list) (at : bool list) (below : bool list) =
    match (above, at, below) with
    | [ _; _ ], [ _; _ ], [ _; _ ] -> []
    | h0 :: h1 :: h2 :: t_ab, i0 :: i1 :: i2 :: t_at, j0 :: j1 :: j2 :: t_bl ->
        let c =
          if i1 then Cell.generate (-1)
          else
            Cell.generate (bool_list_to_int [ h0; h1; h2; i0; i2; j0; j1; j2 ])
        in
        c :: pbl_helper (h1 :: h2 :: t_ab) (i1 :: i2 :: t_at) (j1 :: j2 :: t_bl)
    | [ h1 ], [ i1 ], [ j1 ] -> (
        match (h1, i1, j1) with
        | _ -> raise (Failure {|Single member lists "Internal helper 2"|}))
    | _, _, _ ->
        raise
          (Failure
             {|Improper parsing in Board.parse_boolean_lists "Internal helper 2"|})
  in
  (*Execution*)
  pbl_helper above at below

(**[gen_false_list len] is a boolean list of [false] values of length [len]*)
let rec gen_false_list (len : int) =
  if len <= 0 then [] else false :: gen_false_list (len - 1)

let parse_boolean_grid (grd_in : bool list list) =
  (*Count the number of columns in [grid_in]*)
  let cols =
    match grd_in with
    | h :: _ -> List.length h
    | [] -> 0
  in
  (*Generate a list of false values the same length as cols*)
  let f_list = gen_false_list cols in
  (*Add false list to top and bottom of grid*)
  let grd_app = (f_list :: grd_in) @ [ f_list ] in
  (*Create recursive helper function which parses the grid*)
  let rec parse_grid grd : Cell.cell list list =
    match grd with
    | r0 :: r1 :: r2 :: t ->
        parse_boolean_lists
          ((false :: r0) @ [ false ])
          ((false :: r1) @ [ false ])
          ((false :: r2) @ [ false ])
        :: parse_grid (r1 :: r2 :: t)
    | [ _; _ ] -> []
    | _ -> raise (Failure "Unnaccounted for failure in parse_boolean_grid")
  in
  parse_grid grd_app

(**Counts the number of true elements in a grid of boolean values. Useful for
   counting the number of mines in a board generated from a boolean grid.*)
let count_true_bool_grid (bgrid : bool list list) : int =
  let rec count_true_bool_list blist =
    match blist with
    | [] -> 0
    | h :: t ->
        let tail = count_true_bool_list t in
        if h then 1 + tail else tail
  in
  let rec rec_count_true_bool_grid bg =
    match bg with
    | [] -> 0
    | h :: t -> count_true_bool_list h + rec_count_true_bool_grid t
  in
  rec_count_true_bool_grid bgrid

let generate_random_grid (m : int) (n : int) (ct : int) :
    Cell.cell list list * int =
  Random.self_init ();
  let counter = ref 0 in
  let mx = m * n in
  let grid =
    generate_naive_grid
      (fun _ ->
        if Random.int mx < ct then (
          counter := !counter + 1;
          true)
        else false)
      m n
    |> parse_boolean_grid
  in
  (grid, !counter)

let sum_position (p1 : int * int) (p2 : int * int) : int * int =
  match (p1, p2) with
  | (m1, n1), (m2, n2) -> (m1 + m2, n1 + n2)

let get_offset pos =
  match pos with
  | 1 -> (1, 0)
  | 2 -> (1, 1)
  | 3 -> (0, 1)
  | 4 -> (-1, 1)
  | 5 -> (-1, 0)
  | 6 -> (-1, -1)
  | 7 -> (0, -1)
  | 0 -> (1, -1)
  | _ -> failwith "Bad range in [range_clear]"

let confirm_in_bounds pos m n =
  match pos with
  | m1, n1 -> m1 >= 0 && m1 < m && n1 >= 0 && n1 < n

(*Performs cascade around a known empty cell*)
let rec range_clear (brd : board) (position : int * int) (range : int list) =
  (*Set up ref for later empty checks*)
  let emt = ref false in
  (*[range] determines number of itterations*)
  match range with
  | [] -> brd
  (*if itterations remain*)
  | h :: t ->
      (*check if the proposed position is in bounds of [brd]*)
      let next_check = sum_position position (get_offset h) in
      if confirm_in_bounds next_check brd.m brd.n then
        (*if so then check if proposed position is empty*)
        let bd_poss =
          try
            Some
              {
                brd with
                grid =
                  modify_grid_index
                    (Cell.clear_volatile_cascade emt)
                    next_check brd.grid;
                remainingCells = brd.remainingCells - 1;
              }
          with
          | Cell.MineUncovered -> None
          | Cell.ReclearAttempted -> None
        in
        (*Clear if cell is empty, continue through range otherwise*)
        match bd_poss with
        | None -> range_clear brd position t
        | Some bd_poss ->
            if !emt then
              range_clear
                (range_clear bd_poss next_check [ 0; 1; 2; 3; 4; 5; 6; 7 ])
                position t
            else range_clear bd_poss position t (*Else continue through range*)
      else range_clear brd position t

(*** Functions ****************************************************************)

and clear_position brd (position : int * int) =
  let emt = ref false in
  let bd =
    try
      {
        brd with
        grid =
          modify_grid_index (Cell.clear_volatile_cascade emt) position brd.grid;
        remainingCells = brd.remainingCells - 1;
      }
    with
    | Cell.MineUncovered ->
        {
          brd with
          grid = modify_grid_index Cell.clear position brd.grid;
          validity = false;
        }
    | Cell.ReclearAttempted ->
        emt := false;
        brd
  in
  if not !emt then bd else range_clear bd position [ 0; 1; 2; 3; 4; 5; 6; 7 ]

let flag_position brd (position : int * int) =
  { brd with grid = modify_grid_index Cell.flag position brd.grid }

let to_string_list brd : string list = board_to_stringlist brd

let generate m n =
  match generate_random_grid m n (m * n / 12) with
  | grd, ct ->
      { validity = true; grid = grd; remainingCells = (m * n) - ct; m; n }

let generate_from_bool_grid bool_grd =
  let cols = List.length bool_grd in
  let rows =
    match bool_grd with
    | h :: _ -> List.length h
    | _ -> raise (Failure "Invalid grid input (empty rows)")
  in
  {
    validity = true;
    grid = parse_boolean_grid bool_grd;
    m = cols;
    n = rows;
    remainingCells = (cols * rows) - count_true_bool_grid bool_grd;
  }

let rec int_grid_to_bool_grid int_grid =
  let rec int_list_to_bool_list int_list =
    match int_list with
    | h :: t ->
        (match h with
        | 1 -> true
        | 0 -> false
        | _ -> failwith "Invalid Int Grid")
        :: int_list_to_bool_list t
    | [] -> []
  in
  match int_grid with
  | h :: t -> int_list_to_bool_list h :: int_grid_to_bool_grid t
  | [] -> []

let dimensions brd = (brd.m, brd.n)
let is_valid brd = brd.validity
let is_complete brd = brd.remainingCells = 0
let mines_left brd = brd.remainingCells

let uncover_board (brd : board) : board =
  { brd with grid = mutate_grid Cell.clear brd.grid; validity = false }

(*this is a testing comment to check branches*)
