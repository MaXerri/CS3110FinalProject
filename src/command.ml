type object_phrase = 
| RowCol of int * int 
| LetterCol of string * int

type command =
  | Clear of object_phrase
  | Flag of object_phrase
  | Quit
  | Restart


exception Empty
exception Malformed

let check_empty str =
  List.filter (fun x -> x <> "") (String.split_on_char ' ' str) = []

let check_malformed str_list =
  match str_list with
  | [] -> raise Empty
  | h :: t ->
      if h <> "quit" && h <> "clear" && h <> "flag" && h <> "restart" then true
      else if h = "quit" && t <> [] then true
      else if h = "clear" && t = [] then true
      else if h = "flag" && t = [] then true
      else if h = "restart" && t <> [] then true
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
            Clear
              ( (match int_of_string_opt (List.nth t 0) with
                | Some i -> i
                | None -> raise Malformed),
                match int_of_string_opt (List.nth t 1) with
                | Some i -> i
                | None -> raise Malformed )
          else raise Malformed
        else if h = "flag" then
          if List.length t = 2 then
            Flag
              ( (match int_of_string_opt (List.nth t 0) with
                | Some i -> i
                | None -> raise Malformed),
                match int_of_string_opt (List.nth t 1) with
                | Some i -> i
                | None -> raise Malformed )
          else raise Malformed
        else if h = "quit" then Quit
        else Restart