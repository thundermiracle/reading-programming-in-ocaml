(* forall--MyOwn style *)
let rec forall predicate = function 
  [] -> true
  | a :: rest -> if predicate a then (forall predicate rest)
    else false;;

forall (fun x -> x >= 5) [9;20;5];;
forall (fun x -> x >=5) [6;3;9];;
