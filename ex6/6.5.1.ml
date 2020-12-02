(* 6.5 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

(* 
     x
   x   x
 x  x x  x
*)
let rec comptree x n =
  match n with
  0 -> Lf
  | n' -> Br (x, comptree x (n'-1), comptree x (n'-1));;

comptree "t" 3;;
(*
Br ("t",
    Br ("t", Br ("t", Lf, Lf), Br ("t", Lf, Lf)),
    Br ("t", Br ("t", Lf, Lf), Br ("t", Lf, Lf))
)
*)
