(*  *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

(* 
   1
  2 3
4 5 6 7
*)
let comptree = Br(1, Br(2, Br(4, Lf, Lf), Br(5, Lf, Lf)), Br(3, Br(6, Lf, Lf), Br(7, Lf, Lf)) );;

let rec reflect = function
  Lf -> Lf
  | Br (r, left, right) -> Br (r, reflect right, reflect left);;

reflect comptree;;
(* 
   1
  3 2
7 6 5 4
*)


(* preorder *)
let rec preorder = function
  Lf -> []
  | Br (x, left, right) -> x :: preorder left @ preorder right;;
preorder(reflect comptree);;
(* [1; 3; 7; 6; 2; 5; 4;] *)


(* inorder *)
let rec inorder = function
  Lf -> []
  | Br (x, left, right) -> inorder left @ ( x :: inorder right);;
inorder(reflect comptree);;
(* [7; 3; 6; 1; 5; 2; 4;] *)


(* postorder *)
let rec postorder = function
  Lf -> []
  | Br (x, left, right) -> postorder left @ postorder right @ x :: [];;
postorder(reflect comptree);;
(* [7; 6; 3; 5; 4; 2; 1;] *)
