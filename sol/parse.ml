open! Src.Cs17_setup_hw07

open! Src.Types

(* parse: char list -> datum 

Inputs: a character list 
Outputs: a datum (integer, symbol, or datum list) that corresponds to the characters in 
          the character list 
        
  *)

let string_to_clist s = s |> String . to_seq |> List . of_seq ;;
let remove_spaces chars =
List . filter (fun c -> c <> ' ') chars ;;
let char_list s =
s |> string_to_clist |> remove_spaces ;;

let operator_symbol = function 
    |'+' -> Symbol +
    |'*' -> Symbol *
    | _ -> None 
;;




let rec parse = function
    | [c] -> Integer (int_of_char c - int_of_char 0)
    | ['('; op; a; b; ')' ] -> List [ 
                                    operator_symbol op ; 
                                    Integer (int_of_char a - int_of_char 0); 
                                    Integer (int_of_char b - int_of_char 0) ]
    | _ -> 
