(* 5.8 *)
(* original *)
let rec map f = function
  [] -> []
  | x :: rest -> f x :: map f rest;;


(* 末尾再帰的 *)
let rec fold_left f e l =
  match l with
  [] -> e
  | a :: rest -> fold_left f (f e a) rest;;

let map2 f l = 
  let reversed_results = fold_left (fun l' x -> f x::l') [] l
  in
  fold_left (fun l' x -> x::l') [] reversed_results;;

map2 (fun x -> x * 2) [2;3;4;5];;
