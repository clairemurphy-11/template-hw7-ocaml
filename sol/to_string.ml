open! Src.Cs17_setup_hw07
open! Src.Types
open! Parse
open! Evaluate

(* to_string: value -> string 
inputs: a value 
outputs: the corresponding string *)

let to_string v = match v with NumV n -> string_of_int n;;

check_expect_string (to_string (NumV 13), "13", "NumV single integer");;

check_expect_string (to_string (NumV 0), "0", "NumV zero");;

check_expect_string
  ( to_string (eval (parse (char_list "(* 4 5)"))),
    "20",
    "expression with eval, parse" )
;;

check_expect_string
  ( to_string (eval (parse (char_list "(+ (3 * 2)(2 + 3))"))),
    "11",
    "two nested expressions, parse, eval") ;;
