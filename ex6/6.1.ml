(* 6.1 *)
let similar x y =
  match (x, y) with
  (Point, Point) | (Circle _, Circle _) -> true
  | (Rectangle (l1, w1), Rectangle (l2, w2)) -> l1 = l2 && w1 = w2
  | (Square l1, Square l2) -> l1 = l2
  | _ -> false;;
  
similar (Circle 3) (Circle 5);;
similar (Rectangle (2, 4)) (Rectangle (1, 2));;
similar (Rectangle (1, 2)) (Rectangle (1, 2));;
similar (Square 3) (Square 4);;
similar (Square 3) (Square 3);;
