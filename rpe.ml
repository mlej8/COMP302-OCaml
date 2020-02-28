let even n = (n mod 2) = 0;;
let odd n = (n mod 2) = 1;;

(*Russian peasant exponent algorithm*)
let rec rpe base power = 
  if base = 0 then 0
  else if power = 0 then 1
  else if (odd power) then base * rpe base (power - 1)
  else let tmp = (rpe base (power/2)) in tmp * tmp;;
(* The two recursive calls duplicate all the work. 
   Instead we can use one recursive call
   Runtime changes from order O(n) to order O(logn) *)

print_int (rpe 2 3)

(*Russian peasant exponent algorithm tail recursive*)
let tailrpe base power = 
  let rec helper (base,power,acc) = 
    if base = 0 then 0
    else 
      if power = 0 then acc 
      else helper (base, power-1, acc*base)        
        in
        helper (base, power, 1);;

        
print_int (tailrpe 2 8)