exception NotImplemented;;

type 'a tree =
  Empty | Node of 'a tree * 'a * 'a tree
;;

let deriv((f: float -> float), (dx: float)) =
  fun (x:float) -> (((f (x +. dx)) -. (f x))/.dx)
;;

let iterSum(f, (lo:float), (hi:float), inc) =
  let rec helper((x:float), (result:float)) =
    if (x > hi) then result
    else helper((inc x), (f x) +. result)
  in
  helper(lo,0.0)
;;

let integral((f: float -> float),(lo:float),(hi:float),(dx:float)) =
  let delta (x:float) = x +. dx in
  dx *. iterSum(f,(lo +. (dx/.2.0)), hi, delta)

  
(* Question 1 *)
let t1 = Node(Empty, 0, (Node(Empty, 1, Empty)));;
let t2 = Node(Node(Empty,5,Empty),6,Empty);;
let t3 = Node(Node(t2,2,Node(Empty,3,Empty)),4,t1);;

let mapTree_tests =
  [
    (((fun x -> x + 1), t3), 
     Node (
       Node 
         (Node (Node (Empty, 6, Empty), 7, Empty), 
          3, 
          Node (Empty, 4, Empty)),
       5, 
       Node (Empty, 1, Node (Empty, 2, Empty))
     ));
    
    (((fun x -> x - 1), Empty), Empty);
  ]

let rec mapTree (f, (t: 'a tree)): 'b tree =
  match t with 
  | Empty -> Empty 
  | Node (lt,v,rt) -> Node (mapTree (f,lt), f v, mapTree (f, rt))
;;

(* Question 2. *)

let halfint_tests =
  [
    (((fun x -> x -. 3.0), 5.0,-5.0, 0.01), 3.0078125);
  ]

let rec halfint ((f: float -> float), (posValue : float), (negValue : float), (epsilon : float)) =
  let midpoint = (posValue +. negValue) /. 2.0 in 
  if abs_float(f midpoint) < epsilon then midpoint
  else if (f midpoint) > 0.0 then halfint (f, midpoint, negValue, epsilon)
  else halfint (f, posValue, midpoint, epsilon)
;;

(* Question 3. *)

let newton_tests =
  [
    ((sin,5.0,0.0001,0.0001), 9.42477);
  ]

let rec newton ((f: float -> float),  (guess:float), (epsilon:float), (dx:float)) =
  let close ((x:float), (y:float), (epsilon:float)) = abs_float(x-.y) < epsilon in
  let improve ((guess:float),f,(dx:float)) = (guess -. (f guess /. (deriv (f, dx)) guess)) in
  if close ((f guess), 0.0, epsilon)
  then
    guess
  else
    newton (f, improve (guess, f, dx) , epsilon, dx)
;;

(* Question 4. *)

let indIntegral (f, (dx:float)) =
  fun (x:float): float -> integral (f, 0.0, x,dx)
;;
