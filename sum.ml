(*Algorithm to sum all numbers in an interval*)
let rec sumnums lo hi =
  if lo > hi then 0
  else lo + sumnums (lo+1) hi;;

print_int (sumnums 2 5);;

(* Tail recursive implementation *)
let tailsumnums lo hi = 
  let rec helper (lo, hi, acc) = 
    if lo > hi then acc 
    else helper (lo+1, hi, acc+lo)
    in helper(lo, hi, 0);;

print_int (tailsumnums 2 5);;



