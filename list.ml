(* The basic list operations are:
- "cons" written as :: which adds an element to the front of a list 
- "append", written @ which combines two lists.
Lists in OCaml are homogenous (i.e. all elements in a list must be the same) and immutable. List items are separated by semi-colons.
*)

let vowels = ['a';'e';'i';'o';'u']
let vowels2 = 'y' :: vowels

(* What if we want to add y at the end? vowels :: 'y' gives a type error. *)
let vowels3 = vowels @ ['y'] (* Notice here we had to make 'y' into a list of length 1 by writing ['y']. *)


(* Some functions in the List module. There is a special library (such libraries are called 
"modules" in OCaml.) To use the built-in functions write List.map or List.filter.  Or you can write 
"open List" as I have done below and the List library functions will be available. *)
open List;;

let foo = init 20 (fun n -> n);;
concat [[1;2;3];[4;5;6];[7;8;9]];; (* Concatenate a list of lists. The elements of the argument are all concatenated together (in the same order) to give the result. Not tail-recursive (length of the argument + length of the longest sub-list). *)
nth foo 7;; (* Return the n-th element of the given list. The first element (head of the list) is at position 0.  *)

(* A string to list function; not provided in OCaml. *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) [];;


let l1 = [1;2;3;4;5];;
let l2 = [6;7;8;9;10];;
let l2 = [1;3;5;8;10];;
let liszt = [1; 2; 3; 4; 5]
let liszt2 = [11; 12; 13; 14; 15]
let even n = (n mod 2) = 0;;
let odd n = (n mod 2) = 1;;


(* 1. Function to append two lists in O(n) time *)
let rec append l1 l2 =
  match l1 with 
    | [] -> l2 
    | h::t -> h::(append t l2);;
(* List.iter print_int (append [1;2;3] [4;5;6]);; *)


(* 2. Function to reverse a list done naively. This is O(n^2)
 This algorithm is inefficient with quadratic linear many recursive call. Hence, O(n^2) *)
let rec reverse l = 
  match l with 
  | [] -> [] 
  | h::t -> (reverse t) @ [h];;
(* List.iter print_int (reverse l1);; *)


(* 2.2 Tail recursive version of reversing a list in O(n) *)
let tailrev l =
  let rec helper (l, reversed) = 
    match l with 
    | [] -> reversed
    | x::xs -> helper (xs, x::reversed)
    in 
    helper (l, []);;
(* List.iter print_int (tailrev l1);; *)


(* 3. Function that zips two lists together zip([1;3;5],[2;4;6]) -> [1;2;3;4;5;6] *)
let rec zip (l1,l2) = 
  match l1 with 
  | [] -> l2 
  | x::xs -> x::(zip (l2,xs));;
(* List.iter print_int (zip(l1 ,l2));; *)


(* 3.2 Bad version of the zip function without using pattern-matching 
How do we take lists apart?  OCaml provides "destructors" to take a list apart.  
They are called "hd" and "tl" for "head" and "tail". 
They can be used through pattern-matching instead *)
let v = List.hd liszt
let t = List.tl liszt
let rec badzip (l1,l2)= 
  if l1 = [] then l2 
  else (List.hd(l1))::(badzip(l2, List.tl(l2)));;


(* 4.1 Insert in Insertion Sort
insert: int * int list -> int list expects a SORTED input and produces a SORTED list. *)
let rec insert (n,l) =
  match l with 
  | [] -> [n]
  | x::xs -> 
    if x > n then n::(x::xs) 
    else x::(insert(n,xs));;
(* List.iter print_int (insert (3,[1;2;4;5]));; *)

(* 4.2 Insertion sort *) 
let rec isort l =
  match l with
  | [] -> []
  | x::xs -> insert(x, isort(xs));;         
(* List.iter print_int (isort [8;2;5;1;3;2;6;9;7]);; *)


(* 5. Function that maps a function to every elements of a list 
This function is implemented in OCaml as List.map: apply a given function to every elements of a list.*)
let rec mymap f l = 
  match l with 
  | [] -> []
  | x::xs -> (f x)::(mymap f xs);;
(* List.iter print_int (mymap (fun x -> x + 2) [1;2;3;4]);; *)
(* List.iter print_int (List.map (fun x -> x + 2) [1;2;3;4]);; *)


(* 6. Function that returns a list for each element of a list that returns true for a function 
This function is built-in OCaml as List.filter: returns all the elements of the list l that returns true to function f.
The order of the elements in the input list is preserved.*)
let rec filter f l = 
  match l with 
  | [] -> []
  | x::xs -> if (f x) then x::(filter f xs) else (filter f xs);;
(* List.iter print_int (filter even l1);; *)
(* List.iter print_int (List.filter even l1);; *)


(* 7. Function that initializes a list of length "len" according to a function
This function is implemented in OCaml as List.init: List.init len f is [f 0; f 1; ...; f (len-1)], evaluated left to right. *)
let init len f =
  let rec helper (len, f, l) =     
    if len = 0 then l 
    else helper(len-1, f, (f (len-1))::l) (* since the value of a list at index 0 is 0, shift the len by 1 *)
    in helper (len, f, []);;
(* List.iter print_int (init 5 (fun n -> n + 1));; *)
(* List.iter print_int (List.init 5 (fun n -> n+1)) *)


(* 8. Function that implements fold_left *THE MOST IMPORTANT FUNCTION IN THE LIST MODULE*
Type: ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
This function is implemented in OCaml as List.fold_left: List.fold_left f a [b1; ...; bn] is f (... (f (f a b1) b2) ...) bn. *)
let rec fold_left f acc l = 
  match l with 
  | [] -> acc 
  | x::xs -> fold_left f (f acc x) xs;; (* this is tail recursive, once the recursive call is done, there is nothing left to do. *)
let sumlist l = (fold_left (fun x y -> x + y) 0 l);; (* implementation of sumlist using fold_left *) 
let fold_left_rev l = (fold_left (fun x y -> y::x) [] l);; (* implementation of reverse using fold_left. This function is automatically tail recursive, because fold_left is tail recursive *) 
let concat l = (fold_left (fun a b -> a @ b) [] l);; (* implementation of concat 'a list list -> 'a-list  i.e. [[1;2;3;4;5]] -> [1;2;3;4;5] *)


(* 9. Function that implements fold_right *THIS FUNCTION IS NOT TAIL-RECURSIVE* *)
let rec fold_right f l acc = 
  match l with 
  | [] -> acc
  | x::xs -> f x (fold_right f xs acc)
let newmap f l = fold_right (fun x a -> (f x) :: a) l [];;
let newfilter f l = fold_right (fun x a -> if f x then x :: a else a) l [];;



(*  Below are the results of running the above definitions and expressions through the interpreter.

val vowels : char list = ['a'; 'e'; 'i'; 'o'; 'u']
val vowels2 : char list = ['y'; 'a'; 'e'; 'i'; 'o'; 'u']
val vowels3 : char list = ['a'; 'e'; 'i'; 'o'; 'u'; 'y']
val liszt : int list = [1; 2; 3; 4; 5]
val liszt2 : int list = [11; 12; 13; 14; 15]
val v : int = 1
val t : int list = [2; 3; 4; 5]
val badzip : 'a list * 'a list -> 'a list = <fun>
val foo : int list = [1; 11; 2; 12; 3; 13; 4; 14; 5; 15]
val zip : 'a list * 'a list -> 'a list = <fun>
#   - : int list = [1; 2; 3; 4; 5; 6]
val myappend : 'a list -> 'a list -> 'a list = <fun>
val reverse : 'a list -> 'a list = <fun>
val rev : 'a list -> 'a list = <fun>
#   val foo : int list =
  [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19]
#   - : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9]
#   - : int = 7
val explode : string -> char list = <fun>

 *)