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

let generate i_of_type =
  match i_of_type with
  | -1 -> { c_type = Mine; visibility = Hidden }
  | 0 -> { c_type = Empty; visibility = Hidden }
  | _ -> { c_type = Adjacent i_of_type; visibility = Hidden }

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
      | Adjacent i -> char_of_int i)
  | Flagged -> '?'
  | Hidden -> 'X'

let to_int c =
  match c.c_type with
  | Mine -> -1
  | Empty -> 0
  | Adjacent i -> i

let not_mine c =
  match c.c_type with
  | Mine -> true
  | _ -> false
