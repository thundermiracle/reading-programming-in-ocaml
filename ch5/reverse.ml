(* reverse--MyOwn style *)
let reverse l =
  let rec reverseAppend fromL toL =
    match fromL with
    [] -> toL
    | a :: rest -> reverseAppend rest (a :: toL)
  in
  reverseAppend l [];;

reverse [];;
reverse [1;2;3];;
reverse [[1;2;3]; [4;5]; []; [6;7;8;9;10;]];;
