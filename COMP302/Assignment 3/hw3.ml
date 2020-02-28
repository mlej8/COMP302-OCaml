(* Question 1. *)

let common_tests = [
  (([1; 3; 2; 4; 1; 5; 6; 3], [3; 9; 8; 2; 11; 21; 3]), [3;2]);
  (([0], [1]), []);
  (([],[]),[]);
]

let rec common (l1,l2) : 'a list =
  match (l1,l2) with 
  | (_,[]) -> []
  | ([],_) -> []
  | (x::xs, l) -> 
      if List.mem x l2 
      then x::common(xs,(List.filter (fun e -> e != x) l2))
      else common(xs,l2)
;;

(* Question 2. Mergesort requires that you use recursion.  Using List.sort or
some other sort defeats the whole purpose.  This question is for the
implementation of split.*)

let split_tests = [
  ([], ([], []));
  ([0;1;2;3], ([0;2], [1;3]));
  ([1;2;3], ([1;3], [2]));
]

let rec split l =
  match l with 
  | [] -> ([],[])
  | x::[] -> (x::[],[])
  | f::s::xs -> let (odd, even) = split xs 
      in
      (f::odd, s::even)
;;

(* Question 3 Here you implement merge. *)

let merge_tests = [
  (([0],[1]), [0;1]);
  (([1; 3; 5; 7; 9],[2; 4; 6; 8]), [1; 2; 3; 4; 5; 6; 7; 8; 9]);
  (([1],[0]), [0;1]);
  (([-1;1],[0]), [-1;0;1]);
  (([],[0]), [0]);
  (([],[]), []);
]

let rec merge (l1,l2) =
  match (l1, l2) with 
  | ([],_) -> l2 
  | (_,[]) -> l1 
  | (x::xs,h::t) -> if x < h then x::merge(xs,l2) else h::merge(l1,t)
;;

(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let mergesort_tests = [
  ([1;3;4;5;2;8;7;6], [1;2;3;4;5;6;7;8]);
  ([10;2;8;5;1;4;3;9;7;6],[1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);
  ([1;3;2;4;1;2;5;3],[1; 1; 2; 2; 3; 3; 4; 5]);
  ([],[]);
  ([1],[1]);
]

let rec mergesort l =
  match l with
  | [] -> [] 
  | x::[] -> [x]
  | _ -> let (left,right) = split l in merge(mergesort(left),mergesort(right))
;;
