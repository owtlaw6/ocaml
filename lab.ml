let l = []
let l1 = [2]
let l2 = [2;3;4]

let r2 = [2.5; 4.3]

let l3 = 1::l2

let rec tolist x = 
  if x == 0 then []
  else x :: tolist (x - 1);;
tolist 10;;

let rec tolist x l =
  if x == 0 then l
  else tolist (x-1) (x :: l);;
tolist 10 [];;

let tolist x =
  let rec f x l =
   if x == -1 then l (*ca sa apara si 0*)
   else f (x-1) (x :: l)
  in f x [];;
tolist 10 ;;

(*lista cifrelor unui numar*)
let cifre n = 
  let rec aux n l =
    if n == 0 then l
    else aux (n / 10) ((n mod 10) :: l)
  in aux n [];;
cifre 123;;

let rec sum lista = match lista with
  | h::t -> h + sum t
  | [] -> 0 
(*  | h::[]
  | [h]
  | h1::h2::t
*)
let s  = sum [1;2;3;4;5];;

(*o fct care inmult fiecare elem cu 2*)
let rec dl lista = match lista with 
  | h :: t -> (2 * h) :: dl(t)
  | [] -> []
let r = dl [1; 2; 3];;

let rec map fn lista = match lista with 
  | h :: t -> (fn h) :: (map fn t)
  | [] -> []
let r = map (fun h -> h + 2) [1; 2; 3];;

(*scoate din loista toate elementele pare*)
let rec filtereven lista = match lista with
  | h :: t -> if h mod 2 == 1 then h::(filtereven t)
    else filtereven t
  | [] -> []
let r = filtereven [1; 2; 3];;

let rec filtereven lista = match lista with
  | h :: t when h mod 2 == 0 -> filtereven t
  | h :: t -> h :: (filtereven t)
  | [] -> []
let r = filtereven [1; 2; 3];;

let rec filter predicat lista = match lista with
  | h :: t when predicat h -> filter predicat t
  | h :: t -> h :: (filter predicat t)
  | [] -> []
let r = filter (fun h -> h mod 2 = 0) [1; 2; 3];;

let r = filter (fun h -> h <= 3) [1; 2; 3; 4; 5];; (*elem mai mari decat 3*)

;;