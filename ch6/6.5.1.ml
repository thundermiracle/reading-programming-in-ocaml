(* 無限列 *)
type intseq = Cons of int * (int -> intseq);;

let rec step2 x = Cons(x+2, step2);;
let Cons(x1, f1) = step2 0
let Cons(x2, f2) = f1 x1;;
(* 
2
4
*)

let rec step n x = Cons (x+n, step(n+1));;
let Cons(x1, f1) = step 2 0
let Cons(x2, f2) = f1 x1;;
(* 
2
5
*)

let rec nthseq t (Cons(x, f)) =
  if t = 1 then x
  else nthseq (t-1) (f x);;

nthseq 3 (step 2 0);;
(* 
2
5
9
*)
