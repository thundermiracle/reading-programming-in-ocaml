(* 6.15 *)
type ('a, 'b) sum = Left of 'a | Right of 'b;;

(* ① *)
let f1 (a, s) =
  match s with
  Left b -> Left (a, b)
  | Right c -> Right (a, c);;
f1 (2, Left 3);;

(* ② *)
let f2 = function
  Left (a, b) -> (a, Left b)
  | Right (a, c) -> (a, Right c);;
f2 (Left (2, 3));;


(* ③ *)
let f3 = function
  (Left a, Left c) -> Left (Left (a, c))
  | (Right b, Left c) -> Left(Right(b, c))
  | (Left a, Right d) -> Right(Left(a, d))
  | (Right b, Right d) -> Right(Right(b, d));;
f3 (Left 2, Left 3);;


(* ④ *)
let f4 = function
  Left(Left(a, c)) -> (Left a, Left c)
  | Left(Right(b, c)) -> (Right b, Left c)
  | Right(Left(a, d)) -> (Left a, Right d)
  | Right(Right(b, d)) -> (Right b, Right d);;
f4 (Right(Left(2, 3)));;


(* ⑤ *)
let f5 (f, g) = function
  Left a -> f a
  | Right b -> g b;;
f5 ((fun a -> a + 1), (fun b -> b + 2)) (Left 3);;


(* ⑥ *)
let f6 sumFunc =
  let f a = sumFunc (Left a) in
  let g b = sumFunc (Right b) in
  (f, g);;


(* ⑦ *)
let f7 s a =
  match s with
  Left f -> Left (f a)
  | Right g -> Right (g a);;
