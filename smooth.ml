(* Some smoothing functions. *)
let smooth f delta =
  fun x -> ((f (x -. delta)) +. (f x) +. (f (x +. delta)))/.3.0;;

let smooth2 f dx = fun x -> ((0.1 *. (f (x -. (2.0 *. dx)))) +. (0.2 *. (f (x -. dx)))
                            +. (0.4 *. (f  x)) +. (0.2 *. (f (x +. (2.0 *. dx)))) +.
                              (0.1 *. (f (x +. (2.0 *. dx)))));;