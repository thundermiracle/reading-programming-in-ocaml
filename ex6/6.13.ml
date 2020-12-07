(* 6.13 *)
type intseq = Cons of int * (int -> intseq);;

let rec nthseq t (Cons(x, f)) =
  if t = 1 then x
  else nthseq (t-1) (f x);;

(* x1 -> last fib, x2 -> the one before last fib *)
let rec fibstep x1 x2 = Cons(x1+x2, fibstep x2);;
let fib = fibstep 1 0;;

nthseq 10 fib;;
