(* Example of parsers *)

let example = "(a+(b+(c*d)+e)*f)*g";;
let example2 = "(a+b)*c";;
let example3 = "((a))+b";;
let example4 = "a+b+c+d";;
let example5 = "a+(b+(c+d))";;
let example6 = "((a+b)*(c+d)+e+f*g+h)";;
let badex1 = "a++b";;
let badex2 = "a+(b+c)*2";;
let badex3 = "a+)";;
let badex4 = "a+(b+";;
(*
# parse example;;
- : exptree =
Expr ('*',
 Expr ('+', Var 'a',
  Expr ('*',
   Expr ('+', Var 'b', Expr ('+', Expr ('*', Var 'c', Var 'd'), Var 'e')),
   Var 'f')),
 Var 'g')
# parse example2;;
- : exptree = Expr ('*', Expr ('+', Var 'a', Var 'b'), Var 'c')
# parse example3;;
- : exptree = Expr ('+', Var 'a', Var 'b')
# parse example4;;
- : exptree =
Expr ('+', Var 'a', Expr ('+', Var 'b', Expr ('+', Var 'c', Var 'd')))
# parse example5;;
- : exptree =
Expr ('+', Var 'a', Expr ('+', Var 'b', Expr ('+', Var 'c', Var 'd')))
# parse example6;;
- : exptree =
Expr ('+',
 Expr ('*', Expr ('+', Var 'a', Var 'b'), Expr ('+', Var 'c', Var 'd')),
 Expr ('+', Var 'e', Expr ('+', Expr ('*', Var 'f', Var 'g'), Var 'h')))
 
# let badex1 = "a++b";;
val badex1 : string = "a++b"
# parse badex1;;
Exception: Failure "In primary".
# let badex2 = "a+(b+c)*2";;
val badex2 : string = "a+(b+c)*2"
# parse badex2;;
Exception: Failure "In primary".
# let badex3 = "a+)";;
val badex3 : string = "a+)"
# parse badex3;;
Exception: Failure "In primary".
# let badex4 = "a+(b+";;
val badex4 : string = "a+(b+"
# parse badex4;;
Exception: Invalid_argument "index out of bounds".
# parse "a + b";;
- : exptree = Var 'a'
*)


(* Examples of the code generator. *)

(*


# let example = "(a+(b+(c*d)+e)*f)*g";;
val example : string = "(a+(b+(c*d)+e)*f)*g"
# let t1 = parse example;;
val t1 : exptree =
  Expr ('*',
   Expr ('+', Var 'a',
    Expr ('*',
     Expr ('+', Var 'b', Expr ('+', Expr ('*', Var 'c', Var 'd'), Var 'e')),
     Var 'f')),
   Var 'g')
# codegen t1;;
LOAD  a
STORE 1
LOAD  b
STORE 2
LOAD  c
MUL  d
ADD  e
ADD 2
MUL  f
ADD 1
MUL  g
- : unit = ()
# let example6 = "((a+b)*(c+d)+e+f*g+h)";;
val example6 : string = "((a+b)*(c+d)+e+f*g+h)"
# let t6 = parse example6;;
val t6 : exptree =
  Expr ('+',
   Expr ('*', Expr ('+', Var 'a', Var 'b'), Expr ('+', Var 'c', Var 'd')),
   Expr ('+', Var 'e', Expr ('+', Expr ('*', Var 'f', Var 'g'), Var 'h')))
# codegen t6;;
LOAD  a
ADD  b
STORE 1
LOAD  c
ADD  d
MUL 1
STORE 1
LOAD  e
STORE 2
LOAD  f
MUL  g
ADD  h
ADD 2
ADD 1
- : unit = ()

 *)