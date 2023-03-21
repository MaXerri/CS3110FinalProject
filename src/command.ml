type object_phrase = int * int

type command =
  | Clear of object_phrase
  | Quit

exception Empty
exception Malformed

let check_empty str =
  List.filter (fun x -> x <> "") (String.split_on_char ' ' str) = []

let check_malformed str_list =
  match str_list with
  | [] -> raise Empty
  | h :: t ->
      if h <> "quit" && h <> "clear" then true
      else if h = "quit" && t <> [] then true
      else if h = "clear" && t = [] then true
      else false

let parse str =
  if check_empty str then raise Empty
  else if
    check_malformed
      (List.filter (fun x -> x <> "") (String.split_on_char ' ' str))
  then raise Malformed
  else
    match List.filter (fun x -> x <> "") (String.split_on_char ' ' str) with
    | [] -> raise Empty
    | h :: t ->
        if h = "clear" then
          if List.length t = 2 then
            Clear (int_of_string (List.nth t 0), int_of_string (List.nth t 1))
          else raise Malformed
        else Quit
