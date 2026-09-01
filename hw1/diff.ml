exception NotImplemented;;

type aexp =
| Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let diff : aexp * string -> aexp
=fun (e,x) -> raise NotImplemented;; (* TODO *)