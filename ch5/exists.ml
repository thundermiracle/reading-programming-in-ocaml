(* exists--MyOwn style *)
let rec exists predicate = function
  [] -> false
  | a :: rest -> if predicate a then true
    else exists predicate rest;;

exists (fun x -> (x mod 7) = 0) [23;-98;19;53];;
exists (fun x -> (x mod 7) = 0) [23;19;53];;
