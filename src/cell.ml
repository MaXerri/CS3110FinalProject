type cell

type cell_type =
  | Mine
  | Adjacent of int
  | Empty

type cell_visibility =
  | Cleared
  | Flagged
  | Hidden

let clear = raise (Failure "Unimplemented: Cell.clear")
let to_char = raise (Failure "Unimplemented: Cell.to_char")
