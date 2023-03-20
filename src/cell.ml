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
  visibiblity : cell_visibility;
}

let clear = raise (Failure "Unimplemented: Cell.clear")
let to_char = raise (Failure "Unimplemented: Cell.to_char")
