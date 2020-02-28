(* Expression trees *)

(* Declaring an expression tree type *)
type exptree = Const of int
             | Var of char
             | Plus of exptree * exptree
             | Times of exptree * exptree;;

type binding = char * int;; (* a binding is a char and an int *)
type env = binding list;; (* the type env is a list of bindings *)

(* Declaring variables *)
let e1 = Const 3;;
let e2 = Const 4;;
let e3 = Var 'x';;
let e4 = Var 'y';;
let e5 = Const 5;;
let e6 = Var 'z';;
let e7 = Plus(e1, e2);;
let e8 = Plus(e3, e5);;
let e9 = Times(e6,e8);;
let e10 = Plus(e9,e7);;
let rho: env =  [('x',11);('y',7);('z',2)];;

exception NotFound;;


(* 1. Lookup function *)
let rec lookup name (rho : env) =
  match rho with
  | [] -> None
  | (n,v) :: e -> if name = n then (Some v) else (lookup name e);;


(* 2. Function that evaluates an expression tree *)  
let rec eval (e : exptree) (rho: env) =
  match e with
  | Const n -> n
  | Var v -> (match (lookup v rho) with
             | None -> raise NotFound
             | Some r -> r  )                
  | Plus (e1, e2) ->
      let v1 = (eval e1 rho) in
      let v2 = (eval e2 rho) in
      v1 + v2
  | Times (e1, e2) ->
      let v1 = (eval e1 rho) in
      let v2 = (eval e2 rho) in
      v1 * v2;;
print_int (eval e10 rho);;

      
