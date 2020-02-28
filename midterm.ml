let test1 = [1;1;1;1;1;2;3;3;4;3;3;4;5;6];;
let test2 = [1];;
let test3 = [];;
let test4 = ['a';'b';'a';'a';'a';'a';'a';'a'];;

let rec compress l = 
    match l with 
    | a::b::xs -> if a = b then compress (b::xs) else a::compress(b::xs)
    | e -> e;;

List.iter print_int (compress test1);;
List.iter print_int (compress test2);;
List.iter print_int (compress test3);;
List.iter print_char (compress test4);;

(* Question 2: Implement Convolution *)
(* Question 3: Environment diagram *)

