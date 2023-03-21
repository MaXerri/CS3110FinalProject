type t = {
  current_board : Board.board;
  game_over : bool;
}

let init_state m n = { current_board = Board.generate m n; game_over = false }
let get_current_board st = st.current_board
let is_game_over st = st.game_over

type result =
  | Legal of t
  | Illegal

(*chekcs that both numbers are >0 and chekcs the first # < m and the 2nd number
  < n. Also checks evaluates every input that isnt 2 numbers separated by a
  space to a number outside of the acceptable range so it is detected as a
  incorrect input and no weird errors are thrown *)
let valid_size f st =
  let i = String.split_on_char ' ' f in
  if List.length i <> 2 then false
  else if
    (let x =
       match int_of_string_opt (List.nth i 0) with
       | Some num -> num
       | None -> -1
     in
     x < 0
     || x
        >
        match Board.dimensions st.current_board with
        | a, _ -> a)
    ||
    let y =
      match int_of_string_opt (List.nth i 1) with
      | Some num -> num
      | None -> -1
    in
    y < 0
    || y
       >
       match Board.dimensions st.current_board with
       | _, a -> a
  then false
  else true

let clear input st : result =
  let i = String.split_on_char ' ' input in
  if valid_size input st = false then Illegal
  else
    Legal
      {
        current_board = raise (Failure "Unimplemented: State.clear");
        (*Call Board.clear when implemented*)
        game_over = false (*fix this eventually*);
      }
