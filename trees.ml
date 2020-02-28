(* Define a type tree using a parameterized (or polymorphic) variant (i.e. we are using 'a instead of specified int, float, etc.) *)
type 'a tree = Empty | Node of 'a tree * 'a * 'a tree (* Note Empty and Node are types we define e.g. tree can either be Empty or a Node ('a tree, 'a, 'a tree) *)

let t1 = Node(Empty, 0, (Node(Empty,1,Empty)));;
let t2 = Node(Node(Empty,5,Empty), 6, Empty);;
let t3 = Node(Node(t2, 2,Node(Empty,3,Empty)),4,t1);;
let string_t1 = Node(Empty, "Right Node", (Node(Empty,"Right Right Node",Empty)));;
let string_t2 = Node(Node(Empty,"Left Left Left Node",Empty), "Left Left Node", Empty);;
let string_t3 = Node(Node(string_t2, "Left Node",Node(Empty,"Left Right Node",Empty)),"Root Node",string_t1);;

(* Utility functions *)
let max (m,n) = if m > n then m else n;;
let showInt n = 
  begin
    print_int n; 
    print_string "\n";
  end


(* 1. Function that find the height of a tree *)
let rec height (t:'a tree) = 
  match t with 
  | Empty -> 0
  | Node (left_tree, _ , right_tree) -> 1 + max(height left_tree, height right_tree);;
(* print_int (height t3);; *)


(* 2. inOrder traversal of a tree *)
let rec inOrder (t: 'a tree) = 
  match t with 
  | Empty -> print_string ""
  | Node(l,n,r) -> 
  begin
    (inOrder l);
    (showInt n);
    (inOrder r);
  end;;
(* inOrder t3;; *)


(* 3. preOrder traversal of a tree *)
let rec preOrder (t: 'a tree) = 
  match t with 
  | Empty -> print_string ""
  | Node(l,v,r) -> 
  begin
    showInt v;
    preOrder l;
    preOrder r;
  end;;
(* preOrder t3;; *)


(* 4. postOrder traversal of a tree *)
let rec postOrder (t: 'a tree) = 
  match t with 
  | Empty -> print_string ""
  | Node (l,v,r) -> 
  begin 
    postOrder l;
    postOrder r;
    showInt v;
  end;;
postOrder t3;;


(* 5. Function that flattens a tree *)
let rec flatten (t: 'a tree) = 
  match t with 
  | Empty -> [] 
  | Node (l,v,r) -> v::((flatten l) @ (flatten r));;
(* List.iter print_int (flatten t3);; *)


(* 6. Function that sums the nodes of a tree *)
let rec sumNodes (t: 'a tree) =
  match t with
  | Empty -> 0
  | Node (l,v,r) -> v + (sumNodes l) + (sumNodes r);;
(* print_int (sumNodes t3);; *)


