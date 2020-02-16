exception NotImplemented;;

let remove (item, lst) =
  List.filter (fun u -> not (u = item)) lst

(* Question 1. *)

let common_tests = [
  (([1; 3; 2; 4; 1; 5; 6; 3], [3; 9; 8; 2; 11; 21; 3]), [3;2]);
  (([0], [1]), []);
  (([],[]),[]);
] 
let rec common (l1,l2) = 
  let rec helper (lst1,lst2,acc) = 
    match lst1 with
    | [] -> acc
    | h::t -> if List.mem h lst2 && not (List.mem h acc) then helper(t, lst2, acc @ [h])
        else helper (t,lst2,acc)
  in 
  helper (l1, l2, []);;

(* Question 2. Mergesort requires that you use recursion.  Using List.sort or
some other sort defeats the whole purpose.  This question is for the
implementation of split.

   Given list l is split into two equal (if the length of l is odd then one of the "halves" is one item longer than the other) lists l1 and l2. 

*)

let split_tests = [
  ([5;4;3],([5;3], [4]));
  ([5;3],([5],[3]));
  ([1; 3; 2; 4; 5; 6; 9; 11; 17; 13; 12], ([1; 2; 5; 9; 17; 12], [3; 4; 6; 11; 13]));
]

let rec split l = 
  let rec helper (lst, l1, l2) = 
    match lst with 
    | [] -> (l1,l2)
    | x::[] -> (l1 @ [x], l2)
    | h::i::t -> helper(t, l1 @ [h], l2 @ [i]) 
  in 
  helper(l,[],[]) 

(* Question 3 Here you implement merge. *)

let merge_tests = [(([0],[1]), [0;1]);
                   (([1; 3; 5; 7; 9],[2; 4; 6; 8]), [1; 2; 3; 4; 5; 6; 7; 8; 9]);
                   (([1],[0]), [0;1]);
                   (([-1;1],[0]), [-1;0;1]);
                   (([],[0]), [0]);
                   (([],[]), []);
                  ]

let rec merge (l1,l2) =
  let rec helper(lst1,lst2,list) =
    match lst1 with 
    | [] -> list @ lst2
    | h::t -> match lst2 with 
      | [] -> helper([],[], list @ lst1)
      | h2::t2 -> if h < h2 then helper(t,lst2,list @ [h]) 
          else helper(lst1,t2,list @ [h2]) in
  helper(l1,l2, [])

          

  
(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let mergesort_tests = [
  ([1;3;4;5;2;8;7;6], [1;2;3;4;5;6;7;8]);
  ([10;2;8;5;1;4;3;9;7;6],[1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);
  ([1;3;2;4;1;2;5;3],[1; 1; 2; 2; 3; 3; 4; 5]);
  ([],[]);
  ([1],[1]);
]

let rec mergesort l =
  if (List.length l) > 1 then let (a,b) = split l in
    merge (mergesort a,mergesort b)
  else l
