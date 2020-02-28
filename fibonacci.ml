(*Fibonacci algorithm*)
let rec fib n = 
  if n = 0 then 0
  else if n = 1 then 1
  else (fib (n-1)) +  (fib (n-2));;

print_int (fib 10)

(*Tail recursive Fibonacic algorithm*)
let fastfib n = 
  let rec helper (n, a, b) = 
    if n = 0 then a 
    else if n = 1 then b
    else helper(n-1, b, a+b)
    in 
    helper(n,0,1);;

print_int (fastfib 10);;
