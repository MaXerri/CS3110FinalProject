type t = {
  current_board : Board.board;
  clicked : (int * int) list;
  game_over : bool;
}

let init_state m n =
  { current_board = Board.generate m n; clicked = []; game_over = false }

let get_current_board st = st.current_board
let clicked st = st.clicked
let is_game_over st = st.game_over

type result =
  | Legal of t
  | Illegal

let valid_size f =
  let i = String.split_on_char ' ' f in
  if List.length i <> 2 then false
  else if
    (match int_of_string_opt (List.nth i 0) with
    | Some num -> num
    | None -> -1)
    < 0
    || (match int_of_string_opt (List.nth i 1) with
       | Some num -> num
       | None -> -1)
       < 0
  then false
  else true

let clear input st : result =
  let i = String.split_on_char ' ' input in
  if valid_size input = false then Illegal
  else
    Legal
      {
        current_board = st.current_board;
        (*Board.generate (int_of_string (List.nth i 0)) (int_of_string (List.nth
          i 1));*)
        clicked =
          [ (int_of_string (List.nth i 0), int_of_string (List.nth i 1)) ];
        game_over = false (*fix this eventually*);
      }
