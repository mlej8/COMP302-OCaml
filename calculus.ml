(* Examples of higher-order functions in calculus where the output is also a function *)

(* Utility functions *)
let twice f = fun x -> f (f x);; (* doubler and self-applications *)
let inc n = n + 1;;
let fourtimes f = (twice twice) f;;
let compose (f,g) = fun x -> g (f x);;
let absFl (x:float) = if (x < 0.0) then -.x else x;; 


(* 1. Derivation function *)
let deriv ((f: float -> float), (dx:float)) = (* the type is: (float -> float) * float -> (float -> float) *)
  fun (x: float): float -> ((f (x +. dx)) -. (f x))/. dx;;


(* 2. Iterate sum function *)
let iterSum (f, (lo:float), (hi:float), inc) =
  let rec helper((x:float), (result:float)) =
    if (x > hi) then result
    else helper((inc x), (f x) +. result)
  in
  helper(lo,0.0);;


(* 3. Definite Integral function *)
let integral ((f: float -> float),(lo:float),(hi:float),(dx:float)) =
  dx *. iterSum(f,(lo +. (dx/.2.0)), hi, fun (x: float): float -> x +. dx);;
print_float (integral((fun (x:float):float -> x *. x),0.0,1.0,0.001));;
print_float (integral(sin,0.0, 3.14159, 0.001));;