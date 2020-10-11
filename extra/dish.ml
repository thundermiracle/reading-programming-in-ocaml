(* for learning variant *)
type miso = Aka | Shiro | Awase;;
type gu = Wakame | Tofu | Radish;;
type soup = {m: miso; g: gu};;
type furikake = Shake | Katuo | Nori;;
type dish = PorkCutlet | Soup of soup | Rice of furikake;;


let price_of_dish d =
  match d with
  PorkCutlet -> 350
  | Soup _ -> 90
  | Rice f -> 
    match f with
    Nori -> 80
    | _ -> 90;;

price_of_dish PorkCutlet;;
price_of_dish (Soup {m = Aka; g = Tofu;});;
price_of_dish (Rice Nori);;


let isVeggieDish d =
  match d with
  PorkCutlet -> false
  | Soup _ -> true
  | Rice f -> 
    match f with
    Nori -> true
    | _ -> false;;

isVeggieDish PorkCutlet;;
isVeggieDish (Soup {m = Aka; g = Tofu;});;
isVeggieDish (Rice Katuo);;
