(* 6.8 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;
type 'a rosetree = RLf | RBr of 'a * 'a rosetree list;;

let rec map f = function
  [] -> []
  | x::rest -> f x :: map f rest;;

let rec tree_of_rtree = function
  RLf -> Br (None, Lf, Lf)
  | RBr (r, rtrees) -> Br (Some r, tree_of_rtreelist rtrees, Lf)
  and
  tree_of_rtreelist = function
    [] -> Lf
    | rtree :: rest -> let Br (r, left, Lf) = tree_of_rtree rtree in
                        Br (r, left, tree_of_rtreelist rest);;

let rtree = RBr ("a", [
  RBr ("b", [
    RBr ("c", [RLf]);
    RLf;
    RBr ("d", [RLf]);
    RBr ("e", [RLf]);
    RBr ("f", [RLf])
  ])
]);;
tree_of_rtree rtree;;

(* tree_of_rtree *)
let rec rtree_of_tree = function
  Br (None, Lf, Lf) -> RLf
  | Br (Some r, left, Lf) -> RBr (r, map rtree_of_tree (rtree_of_treelist left))
  and
  rtree_of_treelist = function
    Lf -> []
    | Br (r, left, right) -> Br(r, left, Lf) :: (rtree_of_treelist right);;

rtree_of_tree(tree_of_rtree rtree);;
(* same with rtree *)
