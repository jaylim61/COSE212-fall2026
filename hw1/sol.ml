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

let rec eval f =
  match f with
  | True -> true
  | False -> false
  | Not p -> not (eval p)
  | AndAlso (p, q) -> (eval p) && (eval q)
  | OrElse (p, q) -> (eval p) || (eval q)
  | Imply (p, q) -> (not (eval p)) || (eval q)
  | Equal (p, q) ->
    let rec evalexp e =
      match e with
      | Num n -> n
      | Plus (m, n) -> (evalexp m) + (evalexp n)
      | Minus (m, n) -> (evalexp m) - (evalexp n)
    in (evalexp p) = (evalexp q)

      