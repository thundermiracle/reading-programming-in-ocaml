(* 6.2 *)
type figure =
  Point
  | Circle of float
  | Rectangle of float * float
  | Square of float;;
type 'a with_location = { loc_x: float; loc_y: float; body: 'a; };;

let overlap { loc_x = x1; loc_y = y1; body = body1; } {loc_x = x2; loc_y = y2; body=body2;} = 
  match (body1, body2) with
  | (Point, Point) -> x1 = x2 && y1 = y2
  | (Circle r1, Circle r2)-> (x1 -. x2) ** 2. +. (y1 -. y2) ** 2. <= (r1 +. r2) ** 2.
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
