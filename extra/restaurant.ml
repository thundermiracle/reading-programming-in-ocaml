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

(* 再帰ヴァリアント *)
type menu = Smile | Add of {d: dish; next: menu;};;

let m1 = Smile;;
let m2 = Add {d=PorkCutlet; next=m1;};;
let m3 = Add {d=Soup {m=Aka;g=Tofu;}; next=m2};;
let m4 = Add {d=Rice Nori;next=m3;};;

let rec price_of_menu m =
  match m with
  Smile -> 0
  | Add { d = d'; next = m'; } -> price_of_dish d' + price_of_menu m';;

price_of_menu m4;;

let rec isVeggieMenu m = 
  match m with
  Smile -> true
  | Add { d = d'; next = m'} -> isVeggieDish d' && isVeggieMenu m';;

isVeggieMenu m4;;
