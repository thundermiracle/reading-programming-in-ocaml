(* 7.1 *)
let find x l =
  let rec find' x = function
    [] -> raise Not_found
    | a::rest when a = x -> 1
    | _::rest -> (find' x rest) + 1
  in
  try Some (find' x l) with Not_found -> None;;

find 7 [0;8;7;3;];;

find 9 [0;8;7;3;];;
