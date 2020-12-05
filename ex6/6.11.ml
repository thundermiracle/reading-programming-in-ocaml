(* 6.11 *)
type arith = Const of int | Add of arith * arith | Mul of arith * arith;;

let exp = Mul (Add (Const 3, Const 4), Add (Const 2, Const 5));;

let rec string_of_arith = function
  Const i -> string_of_int i
  | Add (e1, e2) -> "(" ^ string_of_arith e1 ^ "+" ^ string_of_arith e2 ^ ")"
  | Mul (e1, e2) -> string_of_arith e1 ^ "*" ^ string_of_arith e2;;

let rec expand = function
  Const i -> Const i
  | Mul (e1, Add(e2, e3)) -> 
    let (e1', e2', e3') = (expand e1, expand e2, expand e3) in
    Add (expand (Mul (e1', e2')), expand (Mul (e1', e3')))
  | Mul (Add(e1, e2), e3) ->
    let (e1', e2', e3') = (expand e1, expand e2, expand e3) in
    Add (expand (Mul (e1', e3')), expand (Mul (e2', e3')))
  | Mul (e1, e2) -> Mul (expand e1, expand e2)
  | Add (e1, e2) -> Add (expand e1, expand e2);;

string_of_arith exp;;
(* (3+4)*(2+5) *)

string_of_arith (expand exp);;
(* ((3*2+4*2)+(3*5+4*5)) *)
