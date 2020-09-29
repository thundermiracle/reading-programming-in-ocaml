(* fold_right--MyOwn style *)
let rec fold_right f l e =
  match l with
  [] -> e
  | a :: rest -> f a (fold_right f rest e);;

fold_right (fun x y -> x + y) [3;5;7] 0;;
