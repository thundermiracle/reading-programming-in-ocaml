(* 6.2 *)
type figure =
  Point
  | Circle of float
  | Rectangle of float * float
  | Square of float;;
type 'a with_location = { loc_x: float; loc_y: float; body: 'a; };;

(* Only compare same shapes *)
let overlap { loc_x = x1; loc_y = y1; body = body1; } {loc_x = x2; loc_y = y2; body=body2;} = 
  match (body1, body2) with
  | (Point, Point) -> x1 = x2 && y1 = y2
  | (Circle r1, Circle r2)-> (x1 -. x2) ** 2. +. (y1 -. y2) ** 2. <= (r1 +. r2) ** 2.
  | (Rectangle (l1, w1), Rectangle (l2, w2)) -> abs_float (x1 -. x2) <= (l1 +. l2) /. 2. && abs_float (y1 -. y2) <= (w1 +. w2) /. 2.
  | (Square l1, Square l2) -> abs_float (x1 -. x2) <= (l1 +. l2) /. 2. && abs_float (y1 -. y2) <= (l1 +. l2) /. 2.;;
  | _ -> false;;

(* Test Point *)
let p1 = { loc_x = 1.; loc_y = 2.; body = Point; };;
let p2 = { loc_x = 1.; loc_y = 2.; body = Point; };;
let p3 = { loc_x = 2.; loc_y = 2.; body = Point; };;
"Same Point: " ^ string_of_bool (overlap p1 p2);;
(* true *)
"Different Point: " ^ string_of_bool (overlap p1 p3);;
(* false *)

(* Test Circle *)
let c1 = { loc_x = 1.; loc_y = 2.; body = Circle 2.; };;
let c2 = { loc_x = 4.; loc_y = 6.; body = Circle 3.; };;
let c3 = { loc_x = 5.; loc_y = 6.; body = Circle 3.; };;
"Overlap Circle: " ^ string_of_bool (overlap c1 c2);;
(* true *)
"Not Overlap Circle: " ^ string_of_bool (overlap c1 c3);;
(* false *)

(* Test Rectangle *)
let r1 = { loc_x = 1.; loc_y = 2.; body = Rectangle (2., 3.); };;
let r2 = { loc_x = 3.; loc_y = 4.; body = Rectangle (3., 2.); };;
let r3 = { loc_x = 4.; loc_y = 6.; body = Rectangle (3., 2.); };;
"Overlap Rectangle: " ^ string_of_bool (overlap r1 r2);;
(* true *)
"Not Overlap Rectangle: " ^ string_of_bool (overlap r1 r3);;
(* false *)

(* Test Square *)
let s1 = { loc_x = 1.; loc_y = 2.; body = Square 2.; };;
let s2 = { loc_x = 3.; loc_y = 4.; body = Square 3.; };;
let s3 = { loc_x = 4.; loc_y = 6.; body = Square 3.; };;
"Overlap Square: " ^ string_of_bool (overlap s1 s2);;
(* true *)
"Not Overlap Square: " ^ string_of_bool (overlap s1 s3);;
(* false *)
