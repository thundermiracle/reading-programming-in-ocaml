(* 6.5.2 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

(* 
     1
   2   3
 4  5 6  7
8 9 10 11 12 13 14 15
*)
let comptree n =
  let rec comptree' n r = 
    match n with
    0 -> Lf
    | n' -> Br (r, comptree' (n'-1) (2*r), comptree' (n'-1) (2*r+1))
  in
  comptree' n 1;;

comptree 3;;
(* 
Br (1, 
    Br (2, Br (4, Lf, Lf), Br (5, Lf, Lf)),
    Br (3, Br (6, Lf, Lf), Br (7, Lf, Lf))
)
*)
