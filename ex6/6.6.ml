(* 6.6 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

(* 
   1
  2 3
4 5 6 7
*)
let comptree = Br(1, Br(2, Br(4, Lf, Lf), Br(5, Lf, Lf)), Br(3, Br(6, Lf, Lf), Br(7, Lf, Lf)) );;

let rec inorder l = function
  Lf -> l
  | Br (x, left, right) -> inorder (x :: inorder l right) left;;

inorder [] comptree;;
(* [4; 2; 5; 1; 6; 3; 7] *)


let rec postorder l = function
  Lf -> l
  | Br (x, left, right) -> postorder (postorder (x::l) right) left;;

postorder [] comptree;;
(* [4; 5; 2; 6; 7; 3; 1] *)
