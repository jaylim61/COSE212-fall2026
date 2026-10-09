exception NotImplemented;;

(* Problem 1 *)
let prime : int -> bool
= fun n ->
  if n < 2 then false
  else
    let rec check d =
      if d * d > n then true
      else
        if n mod d = 0 then false else check (d+1)
    in check 2

(* Problem 2 *)
let rec range : int -> int -> int list
= fun m n ->
  if m > n then []
  else m::(range (m+1) n)

(* Problem 3 *)
let rec suml : int list list -> int
= fun ll ->
  match ll with
  | [] -> 0
  | hd::tl ->
    let rec sum : int list -> int
    = fun l ->
      match l with
      | [] -> 0
      | hd::tl -> hd + (sum tl)
    in sum hd + suml tl

(* Problem 4 *)
let rec drop : 'a list -> int -> 'a list
= fun l n ->
  match l with
  | [] -> []
  | hd::tl -> if n = 0 then l else drop tl (n-1)

(* Problem 5 *)
let rec max : int list -> int
= fun l ->
  match l with
  | [] -> raise (Failure "Empty list")
  | [e] -> e
  | hd::tl ->
    let maxtl = max tl in
      if hd > maxtl then hd else maxtl

let rec min : int list -> int
= fun l ->
  match l with
  | [] -> raise (Failure "Empty list")
  | [e] -> e
  | hd::tl ->
    let mintl = min tl in
      if hd < mintl then hd else mintl

(* Problem 6 *)
let sigma : (int -> int) -> int -> int -> int
= fun f a b ->
  let rec aux k acc =
    if k > b then acc else aux (k + 1) (acc + f k)
  in aux a 0

(* Problem 7 *)
let rec forall : ('a -> bool) -> 'a list -> bool
= fun p l ->
  match l with
  | [] -> true
  | hd::tl -> p hd && forall p tl

(* Problem 8 *)
let double : ('a -> 'a) -> 'a -> 'a
= fun f n ->
  f (f n)

(* Problem 9 *)
type btree = Empty | Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
  match tree with
  | Empty -> false
  | Node (node, left, right) ->
    n = node || mem n left || mem n right

(* Problem 10 *)
type btree =
| Leaf of int
| Left of btree
| Right of btree
| LeftRight of btree * btree

let rec mirror : btree -> btree
= fun tree ->
  match tree with
  | Leaf _ -> tree
  | Left t -> Right (mirror t)
  | Right t -> Left (mirror t)
  | LeftRight (l, r) -> LeftRight (mirror r, mirror l)

(* Problem 11 *)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun m n ->
  match n with
  | ZERO -> m
  | SUCC k -> natadd (SUCC m) k

let natmul : nat -> nat -> nat
= fun m n ->
  let rec aux k acc =
    match k with
    | ZERO -> acc
    | SUCC i -> aux i (natadd acc m)
  in aux n ZERO

let zero = ZERO
let one = SUCC zero
let two = SUCC one
let three = SUCC two
let four = SUCC three
let five = SUCC four

(* Problem 12 *)
type formula =
| True
| False
| Not of formula
| AndAlso of formula * formula
| OrElse of formula * formula
| Imply of formula * formula
| Equal of exp * exp
and exp =
| Num of int
| Plus of exp * exp
| Minus of exp * exp

let rec evalexp : exp -> int
= fun e ->
  match e with
  | Num n -> n
  | Plus (m, n) -> (evalexp m) + (evalexp n)
  | Minus (m, n) -> (evalexp m) - (evalexp n)

let rec eval : formula -> bool
= fun f ->
  match f with
  | True -> true
  | False -> false
  | Not p -> not (eval p)
  | AndAlso (p, q) -> (eval p) && (eval q)
  | OrElse (p, q) -> (eval p) || (eval q)
  | Imply (p, q) -> (not (eval p)) || (eval q)
  | Equal (p, q) -> (evalexp p) = (evalexp q)

(* Problem 13 *)
type aexp =
| Const of int
| Var of string
| Power of string * int
| Times of aexp list
| Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (exp, var) ->
  match exp with
  | Const c -> Const 0
  | Var x ->
    if x = var then Const 1 else Const 0 
  | Times aexplist -> begin
    match aexplist with
    | [] -> Const 0
    | hd::tl ->
      let f'g = Times [diff (hd, var); Times tl] in
      let fg' = Times [hd; diff (Times tl, var)] in
      Sum [f'g; fg']
    end
  | Sum aexplist -> begin
    match aexplist with
    | [] -> Const 0
    | hd::tl ->
      Sum [diff (hd, var); diff (Sum tl, var)]
    end
  | _ -> raise NotImplemented

  let p = Sum [Power ("x", 2);
              Times [Const 2; Var "x"];
              Const 1]

(* Problem 14 *)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e ->
  let rec eval : exp -> int -> int
    = fun expr x ->
    match expr with
    | X -> x
    | INT n -> n
    | ADD (e1, e2) -> (eval e1 x) + (eval e2 x)
    | SUB (e1, e2) -> (eval e1 x) - (eval e2 x)
    | MUL (e1, e2) -> (eval e1 x) * (eval e2 x)
    | DIV (e1, e2) -> (eval e1 x) / (eval e2 x)
    | SIGMA (e1, e2, e3) ->
      let a = eval e1 x in
      let b = eval e2 x in
      (* eval e3 a + eval e3 a+1 + ... eval e3 b-1 + eval e3 b *)
      let rec sum k acc =
        if k > b then acc
        else sum (k + 1) (acc + eval e3 k)
      in sum a 0
  in eval e 0

(* Problem 15 *)
type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
and var = string

let rec ismem : 'a -> 'a list -> bool
= fun n list ->
  match list with
  | [] -> false
  | hd::tl -> if n = hd then true else ismem n tl

let check : exp -> bool
= fun exp ->
  let rec check_env : exp -> var list -> bool
  = fun e env ->
    match e with
    | V v -> ismem v env
    | P (v, e) -> check_env e (v::env)
    | C (e1, e2) -> check_env e1 env && check_env e2 env
  in check_env exp []

  (* Well-formed progrrams *)
  let p1 = P ("a", V "a")
  let p2 = P ("a", P ("a", V "a"))
  let p3 = P ("a", P ("b", C (V "a", V "b")))
  let p4 = P ("a", C (V "a", P ("b", V "a")))

  (* Ill-formed programs *)
  let p5 = P ("a", V "b")
  let p6 = P ("a", C (V "a", P ("b", V "c")))
  let p7 = P ("a", P ("b", C (V "a", V "c")))