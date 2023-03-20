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

let generate = raise (Failure "Unimplemented: Cell.generate")
let clear = raise (Failure "Unimplemented: Cell.clear")

let to_char c =
  match c.visibility with
  | Cleared -> (
      match c.c_type with
      | Mine -> '!'
      | Empty -> '_'
      | Adjacent i -> char_of_int i)
  | Flagged -> '?'
  | Hidden -> 'X'
