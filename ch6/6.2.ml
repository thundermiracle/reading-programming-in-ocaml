type student = { name: string; id: int; };;

let string_of_student st = st.name ^ "'s ID is" ^ string_of_int st.id;;

type figure =
  Point
  | Circle of int
  | Rectangle of int * int
  | Square of int;;

let c = Circle 3;;
let figs = [Point; Square 5; Rectangle (4, 5); c];;

let area_of_figure = function
  Point -> 0
  | Circle r -> r * r * 3
  | Rectangle (l, w) -> l * w
  | Square w -> w * w;;

map area_of_figure figs;;

let similar x y =
  match (x, y) with
  (Point, Point) | (Circle _, Circle _) | (Square _, Square _) -> true
  | (Rectangle (l1, w1), Rectangle (l2, w2)) -> l1 * w2 = l2 * w1
  | _ -> false;;

similar (Rectangle (2, 4)) (Rectangle (1, 2));;
