exception NotImplemented;;

type typExp =
  | TypInt
  | TypVar of char
  | Arrow of typExp * typExp
  | Lst of typExp;;

type substitution = (char * typExp) list;;
(* We say that two type expressions are unifiable if there is a substitution that makes them identical. *)

let te1 = Arrow(TypInt, Arrow(TypVar 'c', TypVar 'a')) ;;
let te3 = Arrow(TypVar 'a',Arrow (TypVar 'b',TypVar 'c')) ;;

(* Question 1.1 *)
let rec occurCheck (v: char) (tau: typExp) : bool =
  match tau with 
  | TypVar c -> v == c (* Check if a specific type variable occurs in a type expression *)
  | TypInt -> false
  | Lst l -> occurCheck v l
  | Arrow (a,b) -> occurCheck v a || occurCheck v b
;;

(* Question 1.2 *)
let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
  (* replace all occurence of v insde tau2 with tau1 *)
  match tau2 with 
  | TypVar c -> if v == c then tau1 else TypVar c 
  | TypInt -> TypInt
  | Lst l -> Lst (substitute tau1 v l)
  | Arrow (a,b) -> Arrow (substitute tau1 v a, substitute tau1 v b)

(* Question 1.3 *)
let applySubst (sigma: substitution) (tau: typExp) : typExp =
  List.fold_right (fun (c,exp) -> substitute exp c) sigma tau 
;;

(* Question 2 *)
let rec unify (tau1: typExp) (tau2:typExp) : substitution =
  match (tau1, tau2) with 
  | (TypInt, TypInt) -> []  
  | (exp, TypVar char) -> if (TypVar char = exp) then [] else if occurCheck char exp then failwith "Not Unifiable" else [(char, exp)] 
  | (TypVar char,exp) -> if (TypVar char = exp) then [] else if occurCheck char exp then failwith "Not Unifiable" else [(char, exp)]
  | (Lst exp1, Lst exp2) -> unify exp1 exp2
  | (Arrow(a,b), Arrow(c,d)) ->
    let unified = unify a c in 
    let t1 = applySubst unified b in 
    let t2 = applySubst unified d in 
      (unify t1 t2) @ unified
  | _ -> failwith "Not Unifiable"