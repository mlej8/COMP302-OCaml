(* Abstraction using Higher-order Functions *)

(* 1. Function that sums integers in an interval *)
let rec sumInts(a,b) = if (a > b) then 0 else a + sumInts(a+1,b);;

(* 2. Function that sums square of integers in interval *)
let rec sumSquares(a,b) = if (a > b) then 0 else (a*a) + sumSquares(a+1,b);;

(* 3. Function that sums cubes of integers in interval *)
let rec sumCubes(a,b) = if (a > b) then 0 else (a*a*a) + sumCubes(a+1,b);;

(* 4. Function that abstracts the function doing the operation and sums the interval of integers *)
let rec sum(f,lo,hi) =
    if (lo > hi) then 0
    else (f lo) + sum(f,lo+1,hi);;
let square n = n * n;;
let cube n = n * n * n;;
(* print_int (sum (cube,2,4));; *)

(* 5. Function that abstracts how the sequence of integers is incremented *)
let rec sumInc(f,lo,hi,inc) =
    if (lo > hi) then 0
    else (f lo) + sumInc(f, (inc lo), hi, inc);;
let byTwo n = n + 2;;
(* print_int (sumInc (cube,2,4, byTwo));; *)

(* 6. Function that takes the product of sequencial numbers in an interval *)
let rec product(f,lo,hi,inc) =
    if (lo > hi) then 1
    else (f lo) * product(f, (inc lo), hi, inc);;
let id x = x;;
let inc n = n + 1;;
(* print_int (product(id, 1, 5, inc));; (* permutation of 5 *) *)

(* 7. Function that abstracts how each term is combinaned with a combination function *)
let acc(comb,f,lo,hi,inc,unit) =
  let rec helper(a, acc) =
    if (a > hi) then acc
    else
      helper((inc a), comb(acc, (f a)))
    in
        helper(lo, unit);;

(* The following computes sum of the squares of numbers from 1 to 5 *)
print_int (acc((fun (n,m) -> n + m), (fun x -> x * x), 1, 5, (fun n -> n + 1), 0));;

