(* size & depth of tree *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

(* 
    a
  b   c
 d   e f
*)
let chartree = Br ('a', Br('b', Br('d', Lf, Lf), Lf), Br('c', Br('e', Lf, Lf), Br('f', Lf, Lf)));;

let rec size = function
  Lf -> 0
  | Br (_, left, right) -> 1 + size left + size right;;

let rec depth = function
  Lf -> 0
  | Br (_, left, right) -> 1 + max (depth left) (depth right);;

size chartree;;
depth chartree;;
