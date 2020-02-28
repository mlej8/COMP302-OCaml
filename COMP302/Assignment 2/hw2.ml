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
  
  (([],[]),[]);
  (([1; 3; 5],[1; 2; 3]), [(1, 1); (3, 2); (5, 3)]);
  (([1],[3]), [(1,3)]);
]

(* Q1a TODO: Implement pairlists. *)
let rec pairlists (l1,l2) =
  match (l1,l2) with
  | ([],[]) -> []
  | ([],_) -> failwith "Error -- lists are not of the same length"
  | (_,[]) -> failwith "Error -- lists are not of the same length" 
  | (x::xs,h::t) -> (x,h)::pairlists (xs,t)
;;

(* Q1b TODO: Write your own tests for the w_mean function.
         You should NOT test lists of different lengths.
*)
let w_mean_tests = [
  (* Your test cases go here. *)
  (([1.0;2.0;3.0; 2.0], [1.0;2.0;3.0; 1.0]), 2.0);
  (([1.0],[0.0]), 0.0);
  (([1.0],[-1.0]), -1.0);
]

(* Q1b TODO: Implement w_mean. *)
let w_mean (w: float list) (v: float list): float =
  let denom = sumlist w in 
  let pairedlist = pairlists (w,v) in 
  sumlist(List.map (fun (x,y) -> x *. y) pairedlist)/. denom
;;

(* Q2 TODO: Write your own tests for the memberof function. *)
let memberof_tests = [
  (* Your test cases go here. *)
  ((0,[1]),false);
  ((0,[0]),true);
  ((0,[]),false);
]

(* Q2 TODO: Implement memberof. *)
let rec memberof (e,l) =
  match l with
  | [] -> false
  | x::xs -> if e = x then true else memberof(e, xs)
;;

(* Q2 TODO: Write your own tests for the remove function. *)
let remove_tests = [
  (* Your test cases go here. *)
  ((0,[1]),[1]);
  ((0,[0]),[]);
  ((0,[]),[]);
  ((3, [1; 6; 3; 2; 6; 1; 7; 2; 3; 5]), [1; 6; 2; 6; 1; 7; 2; 5]);
  
]

(* Q2 TODO: Implement remove. *)
let rec remove (item, lst) =
  match lst with 
  | [] -> []
  | x::xs -> if item = x then remove(item, xs) else x::remove(item,xs)
;;

(* Q3 TODO: Write your own tests for the find_max function. *)
let find_max_tests = [
  (* Your test cases go here. *)
  ([1; 6; 3; 2; 6; 1; 7; 2; 3; 5], 7);
  ([-1;0], 0);
]

(* Q3 TODO: Implement find_max. *)
let find_max l =
  let head = List.hd l in 
  let rec helper (l,m) = 
    match l with
    | [] -> m
    | x::xs -> if x > m then helper(xs, x) else helper(xs,m)
  in
  helper ((List.tl l), head)
;;

(* Q4 TODO: Write your own tests for the selsort function. *)
let selsort_tests = [
  (* Your test cases go here. *)
  ([],[]);
  ([1;2;3],[3;2;1]);
  ([-1;2;3],[3;2;-1]);
  ([0],[0]);
]

(* Q4 TODO: Implement selsort. *)
let rec selsort l =
  match l with 
  | [] -> [] 
  | _ -> let max = find_max l in 
      max::(selsort (remove (max, l)))
;;
