(* 7.3 *)
let rec change coins amount = 
  match (coins, amount) with
  (_, 0) -> []
  | ((c::rest) as coins, total) -> 
    if c > total then change rest total
    else c::(change coins (total-c));;

let rec change coins amount =
  match (coins, amount) with
    (_, 0) -> []
    | ((c::rest) as coins, total) -> 
      if c > total then change rest total
      else 
        (try
          c::change coins (total - c)
         with Failure "change" -> change rest total
        )
    | _ -> raise (Failure "change");;

let us_coins = [25; 10; 5; 1;]
  and gb_coins = [50;20;10;5;2;1;];;

change gb_coins 43;;
(* [20; 20; 2; 1] *)
change us_coins 43;;
(* [25; 10; 5; 1; 1; 1] *)
change [5;2;] 16;;
(* [5; 5; 2; 2; 2] *)
