(*Factorial algorithm*)
let rec fact n = 
  if n = 0 then 1
  else n * fact(n-1);;

(*Factorial algorithm tail-recursive*)
let fastfact n = 
  let rec helper (n, acc) = 
    if n = 0 then acc 
    else helper (n-1, acc*n)
    in 
    helper (n, 1);;

print_int (fastfact 5);;