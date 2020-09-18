(* 3.15 *)
(* int -> int -> int -> int *)
let func1 a b c=
  a + b + c;;

(* (int -> int) -> int -> int *)
let func2 f a = f a + a;;

(* (int -> int -> int) -> int *)
let func3 f = f 1 2 + 1;;
