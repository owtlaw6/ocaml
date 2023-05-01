(*Velciov Diana grupa 7.2 Randul 2*)
(*problema 1. nr elem dintr-o lista*)

let rec numarare lst i = match lst with
  | h :: t -> numarare t (i + 1)
  | [] -> i;;
let r = numarare [1;2;3;4;5;6] 0;;

let numararefct lst = List.fold_right (fun e p -> (p + 1)) lst 0;;
let r = numararefct [1;2;3;4;5;6;7;8];;

(*problema 2. sumif care calculeaza suma elem pt care f e adevarata*)

(*let sumif lst = List.fold_left (fun p e -> (e + p)) 0 lst;; *)
let sumif lst f = List.fold_left (fun p e -> if f e then (e + p) else p) 0 lst;; 
let r = sumif [1;2;3;4;5] (fun h -> h > 3);;
let r = sumif [1;2;3] (fun h -> h >= 3);;

(*problema 3. calc nr total de km pt masinile opel mai noi de 2010 - fct din list si cu sumif*)

let cars = [
  (*Marca, Km, An*)
  ("Opel", 2589, 2012);
  ("BMW", 11498, 2011);
  ("Fiat", 7748, 2010);
  ("Opel", 14885, 2008);
  ("Fiat", 8996, 2014);
  ("Opel", 78965, 2011)
]

let verif lst = List.fold_right (fun (m, k, a) p -> if (a > 2010 && m == "Opel") then (m, k, a) :: p else p) lst [];;

let r = verif cars;;
