open Sexplib0.Sexp_conv

type datum =
| Integer of int
| Symbol of string
| List of datum list
[@@deriving sexp_of];;

type value = NumV of int [@@deriving sexp_of];;