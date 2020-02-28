(* Utility functions *)

exception NotImplemented;;

let close ((x: float), (y: float)) = abs_float (x -. y) < 0.0001;;

let square (x: float) = x *. x;;

let cube (x:float) = x *. x*. x;;

let odd n = (n mod 2) = 1;;

(* Q1 TODO: Correct these tests for the double function. *)
let double_tests = [
  (0, 0);
  (1, 2);
  (3, 6);
  (9, 18);
]

(* Q1 TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let double (n:int): int = match n with
  | 0 -> 0
  | n -> 2 * n


(* Q1 TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)

let fact_tests = [
  (* Your test cases go here.
     Remember that the outputs of fact should be *floating-point* numbers.
  *)
  (5, 120.0);
  (3, 6.0);
  (0, 1.0);
  (1, 1.0);
  (2, 2.0);
  (10, 3628800.0);
]

(* Q1 TODO: Correct this implementation so that it compiles and returns
the correct answers.
The first colon forces the type of `n` to `int`.
The second one forces the return type of `fact` to be `float`.
*)
let rec fact (n: int): float = match n with 
  | 0 -> 1.0
  | n -> (float_of_int n) *. fact (n-1)
           
           
           
(* Q2 TODO: Write your own tests for the mysqrt function.
You should NOT test cases for n < 0.
*)

let mysqrt_tests = [
  (* Your test cases go here. *)
  (0.0, 0.0);
  (1.0, 1.0);
  (4.0,2.0);
  (81.0, 9.0);
  (100.0,10.0);
]

(* Q2 TODO: Implement mysqrt. *)

let mysqrt (x:float): float = 
  let rec helper(g, x) =
    if close(square(g), x) then g
    else helper(((g +. (x/.g)) /. 2.0), x) in helper(1.0, x);;

(* Q3 TODO: Write your own tests for the cube_root function.
You should NOT test cases for n < 0. *)

let cube_root_tests = [
  (* Your test cases go here. *)
  (0.0, 0.0);
  (1.0, 1.0);
  (8.0, 2.0);
  (27.0, 3.0);
  (125.0, 5.0);
]

(* Q3 TODO: Implement cube_root. *)
let cube_root (x:float): float = 
  let rec helper(g,x) =
    if close(cube(g), x) then g 
    else helper((((2.0 *. g) +. (x /. (g *. g))) /. 3.0), x) in helper(1.1, x);;

(* Q4 TODO: Write your own tests for the fast_exp function.
You should NOT test cases for negative bases or powers. *)

let fast_exp_tests = [
  ((0,0),0);
  ((1,0),1);
  ((2,2),4);
  ((5,3),125);
]

(* Q4 TODO: Implement tail recursive helper fast_exp_aux. *) 
let rec fast_exp_aux(base, power, acc) = 
  if base = 0 then 0
  else if power = 0 then acc 
  else fast_exp_aux(base, power-1, acc*base) 
        
(* Q4 TODO: Implement fast_exp using fast_exp_aux. *)
let fast_exp (base, power) = 
  fast_exp_aux(base, power, 1)