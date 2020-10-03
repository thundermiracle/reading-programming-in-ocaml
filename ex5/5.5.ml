let rec fold_right f l e =
  match l with
  [] -> e
  | a :: rest -> f a (fold_right f rest e);;

(* 5.5.1 *)
let concat l = fold_right (fun l' -> (@) l') l [];;

concat [[0;3;4];[2];[];[5;0]];;


(* 5.5.2 *)
let forall p l = fold_right (fun x -> (&&) (p x)) l true;;

forall (fun x -> x >= 5) [9;20;5];;
forall (fun x -> x >=5) [6;3;9];;


(* 5.5.3 *)
let exists p l = fold_right (fun x -> (||) (p x)) l false;;

exists (fun x -> (x mod 7) = 0) [23;-98;19;53];;
exists (fun x -> (x mod 7) = 0) [23;19;53];;
