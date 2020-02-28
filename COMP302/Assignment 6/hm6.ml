exception NotImplemented;;

type transaction = Withdraw of int | Deposit of int | CheckBalance | ChangePassword of string | Close;;

(* Question 1 *)
open Printf

let makeProtectedAccount ((openingBalance: int), (password: string)) =
  let balance = ref openingBalance in (* balance is a pointer to openingBalance *)
  let p = ref password in 
  let isOpen = ref true in 
  fun ((pswd: string),(t: transaction)) ->
    if !isOpen == false then print_string "Account closed."
    else if !p <> pswd then printf "Incorrect password."
    else
      match t with 
      | Close -> isOpen := false; print_string "Account successfully closed." (* Close the account by changing isOpen boolean to false *)
      | ChangePassword newPassword -> begin 
          p := newPassword;
          print_string "Password changed."
        end
      | Deposit amount -> balance := !balance + amount; printf "The new balance is: %d." !balance
      | CheckBalance -> printf "The balance is: %d." !balance;          
      | Withdraw amount -> if (!balance >= amount) then begin
          balance := !balance - amount; (* if value of balance is greater than the amount user wants to withdraw, assign a new value to the pointer *)
          printf "The new balance is: %d." !balance;
        end
          else print_string "Insufficient funds."        
;;


let zoe = makeProtectedAccount(1000, "BiologyRocks");;
let elisa = makeProtectedAccount(500, "ArtsStudentsArePoor");;
let alison = makeProtectedAccount(2500, "MathIsTheBest");;
elisa("ArtsStudentsArePoor", Withdraw 100);;
alison("MathIsTheBest", Deposit 200);;
zoe("BiologyRocks", CheckBalance);;
zoe("BiologySucks", Withdraw 100);;
elisa("ArtsStudentsArePoor", ChangePassword "CSwillMakeMeRich");;
elisa("ArtsStudentsArePoor", Deposit 100);;
elisa("CSwillMakeMeRich", Deposit 200);;
alison("MathIsTheBest", Close);;
alison("MathIsTheBest", CheckBalance);;