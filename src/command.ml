type object_phrase = int * int

type command =
  | Clear of object_phrase
  | Flag of object_phrase
  | Quit
  | Restart


exception Empty
exception Malformed

let check_empty str =
  List.filter (fun x -> x <> "") (String.split_on_char ' ' str) = []

let int_to_char n =
  let rec int_to_char_aux num (acc_chars : char list) = 
    let remainder = num mod 26 in 
    let quotient = num / 26 in
    if quotient = 0 then
      (Char.chr (remainder + 65)) :: acc_chars
    else
      int_to_char_aux (quotient - 1) ((Char.chr (remainder + 65)) :: acc_chars)
  in
  let char_list = int_to_char_aux n [] in
  char_list |> List.map (fun c -> String.make 1 c) |> String.concat ""

let char_to_int s =
  let rec char_to_int_aux chars acc =
    match chars with
    | [] -> acc
    | c :: t ->
        let code = (Char.code c) - 64 in
        let value = acc * 26 + code  in
        char_to_int_aux t value
  in
  char_to_int_aux (String.to_seq s |> List.of_seq) 0
  


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