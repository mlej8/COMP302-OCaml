(* Square matrix *)

(* We say that a matrix is proper if every row has the same length
We say that a matrix is square if it is proper and the length of each row is equal to the number of rows  *)

open Format
let square_matrix = [[1;2;3]; [4;5;6]; [7;8;9]];;
let square_matrix2 = [[2;3]; [4;6]];;
let not_square_matrix = [[1;2;3]; [4;5;6]; [7;8]];;
let not_square_matrix2 = [[2;3]; [4;5;6]; [7;8]];;

let isSquare (l : int list list): bool = 
    match l with 
    | [] -> true
    | _ -> List.for_all (fun x -> (List.length l) = (List.length x)) l;;

let isSquare2 (l : int list list): bool = 
    let num_rows = List.length l in
        let rec helper l = 
            match l with 
                | [] -> true
                | x::xs -> if (List.length x) = num_rows 
                then helper xs 
                else false
                in helper l
                ;;

printf "%B" (isSquare square_matrix);;  
printf "%B" (isSquare square_matrix2);;    
printf "%B" (isSquare not_square_matrix);;
printf "%B" (isSquare not_square_matrix2);;
printf "%B" (isSquare2 square_matrix);;    
printf "%B" (isSquare2 square_matrix2);;    
printf "%B" (isSquare2 not_square_matrix);;
printf "%B" (isSquare2 not_square_matrix2);;
