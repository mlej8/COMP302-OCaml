exception NotImplemented

(* The type of parsed expression trees *)
type exptree =
  | Var of char
  | Expr of char * exptree * exptree

(* A list of valid variable names *)
let charSet =
  let rec aux i acc =
    if i < 97 then acc else aux (i-1) ((Char.chr i) :: acc)
  in
  aux 122 []

(* Returns true if x is an element of lst, false otherwise *)
let isin (x: char) lst =
  List.exists (fun y -> x = y) lst

(* Question 1: Parsing *)
let parse (inputexp: string): exptree =
  let sym = ref inputexp.[0] in
  let cursor = ref 0 in

  let getsym () =
    cursor := !cursor + 1;
    sym := inputexp.[!cursor]
  in

  let rec expr (): exptree = 
    let aTerm  = term() in 
    if !sym <> '+' then aTerm
    else
      begin
        getsym();
        Expr('+',aTerm, expr()) 
      end
    
  and term (): exptree =
    let aPrimary = primary() in
    if !sym <> '*' then aPrimary
    else 
      begin
        getsym();
        Expr('*',aPrimary,term())
      end
      
  and primary (): exptree =
    if !sym = '('
    then begin
      getsym ();
      let result = expr () in
      if !sym <> ')' then
        failwith "Mismatched parens"
      else if !cursor = (String.length inputexp) - 1  then
        result
      else begin
        getsym ();
        result
      end
    end
    else
    if isin !sym charSet then
      if !cursor = (String.length inputexp) - 1 then
        Var !sym
      else
        let result = Var !sym in
        begin
          getsym ();
          result
        end
    else
      failwith "In primary"
  in
  expr ()

(* Question 2: Code Generation *)
let tempstore = ref 0

let codegen (e: exptree) =
  let rec helper ((e: exptree), (tag: char)) =
    match e with
    | Var c -> 
        if tag  == '*' then Printf.printf "MUL %c\n" c
        else if tag  == '=' then Printf.printf "LOAD %c\n" c
        else Printf.printf "ADD %c\n" c
    | Expr(op, l, r) -> 
        (* recurse in the tree *)
        if tag == '=' then 
          begin helper(l,'='); helper(r,op) end
        else begin tempstore := !tempstore + 1; Printf.printf "STORE %i\n" !tempstore; helper(l,'='); helper(r,op); 
          (if (tag = '+') then
             Printf.printf "ADD %i\n" !tempstore
           else
             Printf.printf "MUL %i\n" !tempstore);
          tempstore := !tempstore - 1
        end
  in
  helper (e, '=')