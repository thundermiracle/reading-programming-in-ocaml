(* 6.10 *)
type arith = Const of int | Add of arith * arith | Mul of arith * arith;;

let exp = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5));;

let rec eval = function
  Const i -> i
  | Add (e1, e2) -> eval e1 + eval e2
  | Mul (e1, e2) -> eval e1 * eval e2;;

eval exp;;
(* (3+4)*(2+5) = 49 *)
