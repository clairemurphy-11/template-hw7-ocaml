open! Src.Cs17_setup_hw07
open! Src.Types

(* parse: char list -> datum 

Inputs: a character list 
Outputs: a datum (integer, symbol, or datum list) that corresponds to the characters in 
          the character list 
        
  *)

let string_to_clist s = s |> String.to_seq |> List.of_seq
let remove_spaces chars = List.filter (fun c -> c <> ' ') chars
let char_list s = s |> string_to_clist |> remove_spaces

let operator_symbol = function
  | '+' -> Symbol "+"
  | '*' -> Symbol "*"
  | _ -> failwith "Invalid operator"

let rec parse_helper chars =
  match chars with
  | [] -> failwith "invalid input"
  | c :: rest when c >= '0' && c <= '9' ->
      (Integer (int_of_char c - int_of_char '0'), rest)
  | '(' :: rest -> (
      match rest with
      | op :: rest1 -> (
          let arg1, after_arg1 = parse_helper rest1 in
          let arg2, after_arg2 = parse_helper after_arg1 in
          match after_arg2 with
          | ')' :: rest2 -> (List [ operator_symbol op; arg1; arg2 ], rest2)
          | _ -> failwith "false")
      | [] -> failwith "false")
  | _ -> assert false

let rec parse = function
  | [ c ] -> Integer (int_of_char c - int_of_char '0')
  | [ '('; op; a; b; ')' ] ->
      List
        [
          operator_symbol op;
          Integer (int_of_char a - int_of_char '0');
          Integer (int_of_char b - int_of_char '0');
        ]
  | _ -> failwith "Not implemented yet"

let parse chars =
  let result, remaining = parse_helper chars in
  match remaining with [] -> result | _ -> failwith "false"
;;

check_expect_datum (parse [ '7' ], Integer 7, "single digit datum");;

check_expect_datum
  ( parse (char_list "( + 3 4)"),
    List [ Symbol "+"; Integer 3; Integer 4 ],
    "addition operator" )
;;

check_expect_datum
  ( parse (char_list "(* (+ 4 6) 5)"),
    List [ Symbol "*"; List [ Symbol "+"; Integer 4; Integer 6 ]; Integer 5 ],
    "nested expression" )
