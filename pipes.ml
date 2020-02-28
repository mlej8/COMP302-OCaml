(* Pipes in OCaml *)
let inc n = n + 1;;
let (--) lo hi = List.init (hi - lo + 1) (fun n -> n + lo);;

[1;2;3] |> List.map inc;;

(1 -- 10) |> List.fold_left (+) 0;;

(1 -- 5) |> List.map (fun n -> n * n) |> List.fold_left (+) 0;;
                               
let compose f g = fun x -> x |> f |> g;;

let (<|) f v = f v;;

(*  @@ is predefined in OCaml to be the same as <| as I just defined it. *)