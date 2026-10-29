open! Src.Cs17_setup_hw07
open! Src.Types
open! Parse

(* eval : datum -> value 

inputs: a datum 
outputs: the value of that datum 

OI: List [ Symbol "*"; Integer 7; Integer 2]
 RI1: Integer 7
 RO1: NumV 7
 RI2: Integer 2
 RO2: NumV 2
operator applied to RO1 and RO2
OO: NumV 14 

*)

let rec eval d =
  match d with
  | Integer n -> NumV n
  | List [ Symbol "+"; arg1; arg2 ] ->
      let (NumV n1) = eval arg1 in
      let (NumV n2) = eval arg2 in
      NumV (n1 + n2)
  | List [ Symbol "*"; arg1; arg2 ] ->
      let (NumV n1) = eval arg1 in
      let (NumV n2) = eval arg2 in
      NumV (n1 * n2)
  | _ -> failwith "false"
;;

check_expect_value (eval (Integer 5), NumV 5, "datum is single integer");;

check_expect_value
  ( eval (List [ Symbol "+"; Integer 3; Integer 7 ]),
    NumV 10,
    "datum is addition expression" )
;;

check_expect_value
  ( eval (List [ Symbol "*"; Integer 2; Integer 4 ]),
    NumV 8,
    "datum is multiplication expression" )
;;

check_expect_value
  ( eval
      (List [ Symbol "*"; List [ Symbol "+"; Integer 2; Integer 5 ]; Integer 3 ]),
    NumV 21,
    "nested expression" )
;;

check_expect_value
  (eval (parse (char_list "5")), NumV 5, "single integer with parse")
;;

check_expect_value
  (eval (parse (char_list "(* 3 4)")), NumV 12, "multiplication with parse")
;;

check_expect_value
  ( eval (parse (char_list "(+ (3 * 2)(2 + 3))")),
    NumV 11,
    "two nested expressions and parse" )
