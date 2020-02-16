exception NotImplemented;;

let rec sumlist l =
  match l with
  | [] -> 0.0
  | x :: xs -> x +. sumlist xs
;;

(* Q1a TODO: Write your own tests for the pairlists function.
         You should NOT test lists of different lengths.
*)
let pairlists_tests = [
  (* Your test cases go here. *)
]

(* Q1a TODO: Implement pairlists. *)
let rec pairlists twolists =
  raise NotImplemented
;;

(* Q1b TODO: Write your own tests for the w_mean function.
         You should NOT test lists of different lengths.
*)
let w_mean_tests = [
  (* Your test cases go here. *)
]

(* Q1b TODO: Implement w_mean. *)
let w_mean weights data =
  raise NotImplemented
;;

(* Q2 TODO: Write your own tests for the memberof function. *)
let memberof_tests = [
  (* Your test cases go here. *)
]

(* Q2 TODO: Implement memberof. *)
let rec memberof pair =
  raise NotImplemented
;;

(* Q2 TODO: Write your own tests for the remove function. *)
let remove_tests = [
  (* Your test cases go here. *)
]

(* Q2 TODO: Implement remove. *)
let rec remove (item, lst) =
  raise NotImplemented
;;

(* Q3 TODO: Write your own tests for the find_max function. *)
let find_max_tests = [
  (* Your test cases go here. *)
]

(* Q3 TODO: Implement find_max. *)
let find_max l =
  raise NotImplemented
;;

(* Q4 TODO: Write your own tests for the selsort function. *)
let selsort_tests = [
  (* Your test cases go here. *)
]

(* Q4 TODO: Implement selsort. *)
let rec selsort l =
  raise NotImplemented
;;
