(* preorder, inorder, postorder *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

(* 
   1
  2 3
4 5 6 7
*)
let comptree = Br(1, Br(2, Br(4, Lf, Lf), Br(5, Lf, Lf)), Br(3, Br(6, Lf, Lf), Br(7, Lf, Lf)) );;

(* preorder *)
let rec preorder = function
  Lf -> []
  | Br (x, left, right) -> x :: preorder left @ preorder right;;

preorder comptree;;
(* 1, 2, 4, 5, 3, 6, 7 *)


(* inorder *)
let rec inorder = function
  Lf -> []
  | Br (x, left, right) -> inorder left @ ( x :: inorder right);;

inorder comptree;;
(* 4,2,5,1,6,3,7 *)


(* postorder *)
let rec postorder = function
  Lf -> []
  | Br (x, left, right) -> postorder left @ postorder right @ x :: [];;

postorder comptree;;
(* 4,5,2,6,7,3,1 *)