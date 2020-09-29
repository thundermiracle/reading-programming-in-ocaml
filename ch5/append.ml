(* append--MyOwn style *)
let rec append fromL toL =
  match fromL with
  [] -> toL
  | a :: rest -> a :: (append rest toL);;

  
append [1;2;3] [4;5;6];;
append [5;6] [];;
append [] [true;false];;
