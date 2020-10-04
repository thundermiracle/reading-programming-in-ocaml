(* 5.6 *)
let randlist = [1458777923.; 1457850878.; 101027544.; 470211272.; 1144108930.; 984943658.; 1622650073.; 282475249.; 16807.; 1.];;

(* originial one *)
let rec quick_sort = function
  ([] | [_]) as l -> l
  | pivot::rest -> 
    let rec partition left right = function
      [] -> (quick_sort left) @ (pivot::quick_sort right)
      | y::ys -> if pivot < y then partition left (y::right) ys
        else partition (y::left) right ys
    in
    partition [] [] rest;;

quick_sort randlist;;


(* no @ version *)
let rec quick_sort2 r = function
  [] -> r
  | [x] -> x::r
  | pivot::rest ->
    let rec partition left right = function
      [] -> quick_sort2 (pivot::quick_sort2 r right) left
      | y::ys -> if pivot < y then partition left (y::right) ys
        else partition (y::left) right ys
    in
    partition [] [] rest;;

quick_sort2 [] randlist;;
