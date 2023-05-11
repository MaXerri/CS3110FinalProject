type g_over =
  | Won
  | Lost
  | Play

type t = {
  current_board : Board.board;
  game_over : g_over;
}

let init_state m n = { current_board = Board.generate m n; game_over = Play }
let get_current_board st = st.current_board

let is_game_over st =
  match st.game_over with
  | Won -> Won
  | Lost -> Lost
  | Play -> Play

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
        >=
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
       >=
       match Board.dimensions st.current_board with
       | _, a -> a
  then false
  else true

let clear input st : result =
  let i = String.split_on_char ' ' input in
  if valid_size input st = false then Illegal
  else
    let b =
      Board.clear_position st.current_board
        (int_of_string (List.nth i 0), int_of_string (List.nth i 1))
    in
    Legal
      {
        current_board = b;
        (* Switch to just falses for testing purposes*)
        game_over =
          (if Board.is_complete b then Won
          else if Board.is_valid b = false then Lost
            (*should be not is_valid but there is some error atm*)
          else Play);
      }

let flag input st : result =
  let i = String.split_on_char ' ' input in
  if valid_size input st = false then Illegal
  else
    Legal
      {
        current_board =
          Board.flag_position st.current_board
            (int_of_string (List.nth i 0), int_of_string (List.nth i 1));
        game_over = Play;
      }
