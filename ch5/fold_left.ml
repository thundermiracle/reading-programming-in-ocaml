(* fold_left--MyOwn style *)
let rec fold_left f e l =
  match l with
  [] -> e
  | a :: rest -> fold_left f (f e a) rest;;

fold_left (fun x y -> x - y) 0 [3;5;7];;
fold_left (fun x y -> y :: x) [] [1;2;3];;
