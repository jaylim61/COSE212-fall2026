(* Do not use any module *)

(***********************)
(*  Library functions  *)
(***********************)

let rec fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
= fun f accu lst ->
  match lst with
  | [] -> accu
  | hd::tl -> fold_left f (f accu hd) tl

let rec map : ('a -> 'b) -> 'a list -> 'b list
= fun f lst ->
  match lst with
  | [] -> []
  | hd::tl -> (f hd)::(map f tl)

(***********************)
(******  Syntax  *******)
(***********************)

type program = exp
and exp = 
  | UNIT
  | TRUE
  | FALSE
  | CONST of int
  | VAR of var
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | NIL
  | CONS of exp * exp      
  | APPEND of exp * exp
  | HEAD of exp
  | TAIL of exp
  | ISNIL of exp
  | IF of exp * exp * exp
  | LET of var * exp * exp
  | LETREC of var * var * exp * exp
  | LETMREC of (var * var * exp) * (var * var * exp) * exp
  | PROC of var * exp
  | CALL of exp * exp
  | PRINT of exp
  | SEQ of exp * exp
and var = string

type value = 
  | Unit 
  | Int of int 
  | Bool of bool 
  | List of value list
  | Procedure of var * exp * env 
  | RecProcedure of var * var * exp * env
  | MRecProcedure of var * var * exp *
                     var * var * exp * env
and env = (var * value) list

exception UndefinedSemantics

let rec string_of_value v = 
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | List lst -> "[" ^ fold_left (fun s x -> s ^ ", " ^ x) "" (map string_of_value lst) ^ "]"
  | _ -> "(functional value)"

let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec lookup_env x e = 
  match e with
  | [] -> raise (Failure ("variable " ^ x ^ " is not bound in env")) 
  | (y,v)::tl -> if x = y then v else lookup_env x tl

let rec eval : exp -> env -> value
= fun exp env ->
  match exp with
  | UNIT -> Unit
  | TRUE -> Bool true
  | FALSE -> Bool false
  | CONST n -> Int n
  | VAR v -> lookup_env v env
  | ADD (e1, e2) -> (
    match ((eval e1 env), (eval e2 env)) with
    | (Int m, Int n) -> Int (m + n)
    | _ -> raise UndefinedSemantics
    )
  | SUB (e1, e2) -> (
    match ((eval e1 env), (eval e2 env)) with
    | (Int m, Int n) -> Int (m - n)
    | _ -> raise UndefinedSemantics
    )
  | MUL (e1, e2) -> (
    match ((eval e1 env), (eval e2 env)) with
    | (Int m, Int n) -> Int (m * n)
    | _ -> raise UndefinedSemantics
    )
  | DIV (e1, e2) -> (
    match ((eval e1 env), (eval e2 env)) with
    | (Int m, Int n) when n <> 0 -> Int (m / n)
    | _ -> raise UndefinedSemantics
    )
  | EQUAL (e1, e2) -> (
    match ((eval e1 env), (eval e2 env)) with
    | (Int m, Int n) -> Bool (m = n)
    | (Bool p, Bool q) -> Bool (p = q)
    | (Unit, Unit) -> Bool true
    | _ -> raise UndefinedSemantics
    )
  | LESS (e1, e2) -> (
    match ((eval e1 env), (eval e2 env)) with
    | (Int m, Int n) -> Bool (m < n)
    | _ -> raise UndefinedSemantics
    )
  | NOT b -> (
    match (eval b env) with
    | Bool p -> Bool (not p)
    | _ -> raise UndefinedSemantics
    )
  | NIL -> List []
  | CONS (e1, e2) -> (
    match (eval e1 env, eval e2 env) with
    | (v, List s) -> List (v :: s)
    | _ -> raise UndefinedSemantics
    (* This list allows mixed types *)
    )
    (*
    let v = eval e1 env in
    let s = eval e2 env in
    match s with
    | List s -> List (v :: s)
    | _ -> raise UndefinedSemantics
    *)
  | APPEND (e1, e2) -> (
    match ((eval e1 env), (eval e2 env)) with
    | (List s1, List s2) -> List (s1 @ s2)
    | _ -> raise UndefinedSemantics
    )
  | HEAD e ->
    let l = eval e env in (
      match l with
      | List (hd :: tl) -> hd
      | _ -> raise UndefinedSemantics
    )
  | TAIL e -> 
    let l = eval e env in (
      match l with
      | List (_ :: tl) -> List tl
      | _ -> raise UndefinedSemantics
    )
  | ISNIL e -> begin
    match (eval e env) with
    | List [] -> Bool true
    | List l -> Bool false
    | _ -> raise UndefinedSemantics
    end
  | IF (e1, e2, e3) -> begin
    match (eval e1 env) with
    | Bool true -> eval e2 env
    | Bool false -> eval e3 env
    | _ -> raise UndefinedSemantics
    end
  | LET (x, e1, e2) ->
    eval e2 (extend_env (x, (eval e1 env)) env)
  | LETREC (f, x, e1, e2) ->
    eval e2 (extend_env (f, RecProcedure (f, x, e1, env)) env)
  | LETMREC ((f, x, e1), (g, y, e2), e3) ->
    eval e3
    (extend_env (f, MRecProcedure (f, x, e1, g, y, e2, env))
    (extend_env (g, MRecProcedure (g, y, e2, f, x, e1, env)) env))
  | PROC (x, e) -> Procedure (x, e, env)
  | CALL (e1, e2) -> (
    match (eval e1 env) with
    | Procedure (x, e, env') ->
      let v = eval e2 env in
        eval e (extend_env (x, v) env')
    | RecProcedure (f, x, e, env') ->
      let v = eval e2 env in
        eval e
        (extend_env (x, v)
        (extend_env (f, RecProcedure (f, x, e, env')) env))
    | MRecProcedure (f, x, ef, g, y, eg, env') ->
      let v = eval e2 env in
        eval ef
        (extend_env (x, v)
        (extend_env (f, MRecProcedure (f, x, ef, g, y, eg, env'))
        (extend_env (g, MRecProcedure (g, y, eg, f, x, ef, env')) env)))
    | _ -> raise UndefinedSemantics
    ) 
  | PRINT e -> (print_endline (string_of_value (eval e env)); Unit)
  | SEQ (e1, e2) -> eval e1 env; eval e2 env;
  | _ -> raise (Failure "Not implemented") (* TODO *)

let runml : program -> value
= fun pgm -> eval pgm empty_env

(***********************)
(**  Example programs **)
(***********************)

(*
  let x = 1
  in let f = proc (y) (x + y)
     in let x = 2
        in let g = proc (y) (x + y)
        in  (f 1) + (g 1)
*)
let let1 = 
  LET ("x", CONST 1, 
    LET ("f", PROC ("y", ADD (VAR "x", VAR "y")),
      LET ("x", CONST 2, 
         LET ("g", PROC ("y", ADD (VAR "x", VAR "y")), 
            (ADD (CALL (VAR "f", CONST 1), 
                  CALL (VAR "g", CONST 1)))))))

(*
  letrec double(x) = if (x = 0) then 0 else (double (x-1) + 2
  in (double 6)
*)
let double = 
  LETREC ("double", "x", IF (EQUAL (VAR "x", CONST 0), 
                            CONST 0, 
                            ADD (CALL (VAR "double", SUB (VAR "x", CONST 1)) , 
                                 CONST 2)), 
    CALL (VAR "double", CONST 6))

(*
  letrec even(x) = if (x = 0) then true else odd(x-1)
         odd(x) = if (x = 0) then false else even(x-1)
  in (odd 13)
*)
let evenodd = 
  LETMREC (("even", "x", IF (EQUAL (VAR "x", CONST 0), TRUE, CALL (VAR "odd",  SUB (VAR "x", CONST 1)))),
           ("odd" , "x", IF (EQUAL (VAR "x", CONST 0), FALSE, CALL (VAR "even", SUB (VAR "x", CONST 1)))),
  CALL (VAR "odd", CONST 13))

(*
letrec factorial(x) = 
         if (x = 0) then 1 
         else factorial(x-1) * x
in letrec loop n = 
     if (n = 0) then ()
     else (print (factorial n); loop (n-1))
   in (loop 10)
*)
let fact = 
LETREC ("factorial", "x", 
          IF (EQUAL (VAR "x", CONST 0), CONST 1, 
              MUL (CALL (VAR "factorial", SUB (VAR "x", CONST 1)), VAR "x")), 
  LETREC ("loop", "n", 
    IF (EQUAL (VAR "n", CONST 0), UNIT, 
        SEQ (PRINT (CALL (VAR "factorial", VAR "n")), 
             CALL (VAR "loop", SUB(VAR "n", CONST 1)))), 
      CALL (VAR "loop", CONST 10)))
           
(*
in letrec range(n) = 
      if (n = 1) then (cons 1 nil)
      else n::(range (n-1))
in (range 10)
*)
let range = 
LETREC ("range", "n", 
            IF (EQUAL (VAR "n", CONST 1), CONS (CONST 1, NIL),
                CONS (VAR "n", CALL (VAR "range", SUB (VAR "n", CONST 1)))), 
     CALL (VAR "range", CONST 10))

(*
letrec reverse(l) = 
  if (isnil l) then []
  else (reverse (tl l)) @ (cons hd l)
in (reverse (cons (1, cons (2, cons (3, nil)))))
*)
let reverse = 
LETREC ("reverse", "l", 
          IF (ISNIL (VAR "l"), NIL, 
              APPEND (CALL (VAR "reverse", TAIL (VAR "l")), 
                      CONS (HEAD (VAR "l"), NIL))), 
     CALL (VAR "reverse", 
           CONS (CONST 1, CONS (CONST 2, CONS (CONST 3, NIL)))))

let zfact = 
  LET ("fix", 
    PROC ("f", 
      CALL (PROC ("x", CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))), 
            PROC ("x", CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))))),
    LET ("f", CALL (VAR "fix", 
            PROC ("f", PROC ("x", 
          IF (EQUAL (VAR "x", CONST 0), CONST 1, 
              MUL (CALL (VAR "f", SUB (VAR "x", CONST 1)), VAR "x"))))), 
           CALL (VAR "f", CONST 10)))

let zrange = 
  LET ("fix", 
    PROC ("f", 
      CALL (PROC ("x", CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))), 
            PROC ("x", CALL (VAR "f", PROC ("y", CALL (CALL (VAR "x", VAR "x"), VAR "y")))))),


    LET ("f", CALL (VAR "fix", 
            PROC ("range", PROC ("n", 
               IF (EQUAL (VAR "n", CONST 1), CONS (CONST 1, NIL),
                 CONS (VAR "n", CALL (VAR "range", SUB (VAR "n", CONST 1))))))), 
           CALL (VAR "f", CONST 10)))
