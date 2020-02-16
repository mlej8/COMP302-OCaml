exception NotImplemented;;

type term = Term of float * int;;

type poly = Poly of (float * int) list;;

exception EmptyList;;

(*
Notice that this is a mutually recursive definition. 
Each type mentions the other one. 
The keyword "and" is used for mutually recursive definitions.
*)

type cell = { data : int; next : rlist}
and rlist = cell option ref;;

(* This converts an rlist to an ordinary list. *)
let rec displayList (c : rlist) =
  match !c with
    | None -> []
    | Some { data = d; next = l } -> d :: (displayList l);;

let cell2rlist (c:cell):rlist = ref (Some c);;

let bigger ((x: int), (y: int)) = x > y;;

(* Q1 Polynomials TODO: Implement the following four functions *)

let multiplyPolyByTerm (Term(c,e), Poly p) =
  let rec helper ((coef, deg), p) = 
    match ((coef,deg), p) with 
    | ((coef,deg),[]) -> raise EmptyList
    | ((0.0,0),_) ->  [(0.0, 0)]
    | ((_,_),(coefficient, degree)::[]) -> [(coefficient*.coef, deg + degree)]
    | ((_,_),(coefficient, degree)::rest) -> (coefficient*.coef, deg + degree)::helper((coef,deg), rest) 
  in 
  Poly(helper ((c,e), p))
;;

let addTermToPoly (Term(c,e), Poly p) =
  let rec helper ((coef, deg), p) = 
    match p with 
    | [] -> [(coef,deg)] 
    | (coefficient, degree)::rest -> 
        if deg = 0 && coef = 0.0 then p
        else if deg = degree then (coefficient+.coef, degree)::rest
        else if deg > degree then (coef,deg)::(coefficient,degree)::rest
        else (coefficient,degree)::helper((coef,deg),rest)
  in 
  Poly (helper ((c,e),p))
;;

let addPolys (Poly p1, Poly p2) =
  let rec helper (Poly poly1, Poly poly2) = 
    match (poly1,poly2) with
    | ([],_) -> poly2
    | (_,[]) -> poly1
    | ((coef,deg)::xs,poly2) -> helper(Poly xs, addTermToPoly(Term (coef,deg),Poly poly2))
  in 
  Poly (helper (Poly p1,Poly p2))
;;

let multPolys (Poly p1, Poly p2) =
  let rec helper (Poly poly1, Poly poly2) = 
    match (poly1,poly2) with 
    | ([],_) -> Poly [] (* when there are no more terms, add nothing to previous polynomial *)
    | ((coef,deg)::rest, poly2) -> addPolys (helper(Poly rest, Poly poly2),multiplyPolyByTerm(Term(coef,deg), Poly poly2))
  in helper(Poly p1, Poly p2)
;;

(* Q2 References TODO: implement the `insert` function 
 inserts an element into a sorted linked list and preserves the sorting. 
*)

let rec insert comp (item: int) (list:rlist) =
  match !list with 
  | None -> list := Some {data=item; next=ref None} (* make list point to new_cell *) 
  | Some {data=d; next=l} -> if comp (item,d) then begin
      list := Some {data=item; next=l};
      insert comp d l
    end
      else insert comp item l
;;

