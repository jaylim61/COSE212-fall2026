exception NotImplemented;;

(* Problem 1 *)
let prime n = (* int -> bool *)
  if n < 2 then false
  else
    let rec check d =
      if d * d > n then true
      else
        if n mod d = 0 then false else check (d+1)
    in check 2

(* Problem 2 *)
let rec range n m = (* int -> int -> int list *)
  if n > m then []
  else n::(range (n+1) m)

(* Problem 3 *)
let rec suml ll = (* int list list -> int *)
  match ll with
  | [] -> 0
  | hd::tl -> List.fold_left (+) 0 hd + suml tl

(* one-line solution by Gemini *)
let suml ll =
  List.fold_left (fun acc l -> acc + List.fold_left (+) 0 l) 0 ll

(* Problem 4 *)
let rec drop l n = (* 'a list -> int -> 'a list *)
  match l with
  | [] -> []
  | hd::tl -> if n = 0 then l else drop tl (n-1)

(* Problem 5 *)
let rec max l = (* int list -> int *)
  match l with
  | [] -> raise (Failure "empty list")
  | [e] -> e
  | hd::tl ->
    let maxtl = max tl in
      if hd > maxtl then hd else maxtl

(* Problem 6 *)
let sigma f a b = (* (int -> int) -> int -> int -> int *)
  let rec sum l acc =
    match l with
    | [] -> acc
    | hd::tl -> sum tl (acc+hd)
  in sum (List.map f (range a b)) 0

(* Gemini's suggestion *)
let sigma f a b =
  let rec aux k acc =
    if k > b then acc
    else aux (k + 1) (acc + f k)
  in
  aux a 0

(* Problem 7 *)
let rec forall p l = (* ('a -> bool) -> 'a list -> bool *)
  match l with
  | [] -> true
  | hd::tl -> p hd && forall p tl

(* Problem 8 *)
let double f n = (* ('a -> 'a) -> 'a -> 'a *)
  f (f n)

(* Problem 9 *)
type btree = Empty | Node of int * btree * btree

let rec mem n tree = (* int -> btree -> bool *)
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

let rec mirror tree = (* btree -> btree *)
  match tree with
  | Leaf _ -> tree
  | Left t -> Right (mirror t)
  | Right t -> Left (mirror t)
  | LeftRight (l, r) -> LeftRight (mirror r, mirror l)

(* Problem 11 *)
type nat = ZERO | SUCC of nat

let rec natadd m n = (* nat -> nat -> nat *)
  match n with
  | ZERO -> m
  | SUCC k -> natadd (SUCC m) k

let natmul m n = (* nat -> nat -> nat *)
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

let rec evalexp e =
  match e with
  | Num n -> n
  | Plus (m, n) -> (evalexp m) + (evalexp n)
  | Minus (m, n) -> (evalexp m) - (evalexp n)

let rec eval f = (* formula -> bool *)
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
  