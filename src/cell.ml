type cell_type =
  | Mine
  | Adjacent of int
  | Empty

type cell_visibility =
  | Cleared
  | Flagged
  | Hidden

type cell = {
  c_type : cell_type;
  visibility : cell_visibility;
}

exception MineUncovered
exception IntegerInputOutOfRange
exception ReclearAttempted

let generate i_of_type =
  match i_of_type with
  | -1 -> { c_type = Mine; visibility = Hidden }
  | 0 -> { c_type = Empty; visibility = Hidden }
  | a ->
      if a > 0 && a < 9 then
        { c_type = Adjacent i_of_type; visibility = Hidden }
      else raise IntegerInputOutOfRange

let clear c = { c with visibility = Cleared }

let flag c =
  match c.visibility with
  | Hidden -> { c with visibility = Flagged }
  | Flagged -> { c with visibility = Hidden }
  | Cleared -> c

let to_char c =
  match c.visibility with
  | Cleared -> (
      match c.c_type with
      | Mine -> '!'
      | Empty -> '_'
      | Adjacent i -> char_of_int (i + 48))
  | Flagged -> '?'
  | Hidden -> 'X'

let to_int c =
  match c.c_type with
  | Mine -> -1
  | Empty -> 0
  | Adjacent i -> i

let not_mine c =
  match c.c_type with
  | Mine -> false
  | _ -> true

let is_empty c =
  match c.c_type with
  | Empty -> true
  | _ -> false

let clear_volatile c =
  if not_mine c then { c with visibility = Cleared } else raise MineUncovered

let clear_volatile_cascade (emt : bool ref) (c : cell) : cell =
  if not_mine c then (
    match c.visibility with
    | Cleared -> raise ReclearAttempted
    | _ ->
        emt := is_empty c;
        { c with visibility = Cleared })
  else raise MineUncovered
