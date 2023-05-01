let lista = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

let l = List.map (fun e -> e * 2) lista;;

let l = List.filter (fun h -> h > 4) lista;;

let grthen4 lista1 = List.filter (fun h -> h > 4) lista1

let r = grthen4 [1;2;3;4;5;6;7];;
let r1 = grthen4 lista;;

let grthen4 = List.filter (fun h -> h > 4)
let r = grthen4 [1;2;3;4;5;6;7];;
let r1 = grthen4 lista;;

let rec sumaelem lst = match lst with
  | h :: t -> h + sumaelem t
  | [] -> 0
let r = sumaelem lista;;

let rec sumaelem reducer lst = match lst with
  | h :: t -> reducer h (sumaelem reducer t)
  | [] -> 0
let r = sumaelem (fun e p -> e + p) [1;2;3;4];;
let r1 = sumaelem (fun e p -> e * p) lista;;

let rec sumaelem reducer initval lst = match lst with
  | h :: t -> reducer h (sumaelem reducer initval t)
  | [] -> initval
let sum = sumaelem (fun e p -> Printf.printf "%d \n" e;  e + p) 0 [1;2;3;4];;
let prod = sumaelem (fun e p -> e * p) 1 [1;2;3];;

let r1 = sumaelem (fun e p -> e :: p) [] [1;2;3];;

let sumaelem reducer initval lst = 
  let rec aux lst p = match lst with
  | h :: t -> aux t (reducer h p)
  | [] -> p
  in aux lst initval;;
let r1 = sumaelem (fun e p -> e :: p) [] [1;2;3]

let d = List.fold_left (fun p e -> p + e) 0 [1;2;3]

let nrelem lst = List.fold_left (fun p e -> p + 1) 0 lst
let r = nrelem [1;2;3;4];;

let doubleall lst = List.fold_left (fun p e -> e * 2 :: p) [] lst
let r = doubleall [1;2;3;4];;

let doubleall1 op lst = List.fold_left (fun p e -> (op e) :: p) [] lst
let r = doubleall1 (fun e -> e * 2) [1;2;3;4];;

let doubleall1 op lst = List.fold_right (fun e p -> (op e) :: p) lst []
let r = doubleall1 (fun e -> e * 2) [1;2;3;4];;

let sth lst = List.fold_left (fun p e -> if (e > 2) then e :: p else p) [] lst
let r = sth [1;2;3;4;5];;
let rr = sth r;;

let sth lst = List.fold_right (fun e p -> if e mod 2 == 0 then e :: p else p) lst []
let r = sth [1;2;3;4;5;6];;

let sth pred lst = List.fold_right (fun e p -> if pred e then e :: p else p) lst []
let r = sth (fun x -> x > 3) [1;2;3;4;5;6];;

let med lst = float_of_int (List.fold_left ( + ) 0 lst) /. float_of_int (List.length lst)
let rr = med[1;2;3;4];;

(*tuple*)

let p1 = (10., 1.)
let p2 = (5., 2.)

let dist p1 p2 = 
  let (x1, y1) = p1 in
  let (x2, y2) = p2 in
  Printf.printf "x1 = %f, y1 = %f, x2 = %f, y2 = %f" x1 y1 x2 y2

let r = dist p1 p2;;

let med lst = List.fold_left (fun (suma, count) e -> (suma + e, count +1) ) (0, 0) lst
let r = med [1;2;3;4];;


let med lst = let (suma, count) = 
  List.fold_left (fun (suma, count) e -> (suma + e, count +1) ) (0, 0) lst
  in float_of_int suma /. float_of_int count
let r = med [1;2;3;4];;

let person = [
  ("Alex", 20, "Timis");
  ("Stefan", 22, "Arges");
  ("Alex", 25, "Ilfov");
  ("Bogdan", 27, "Timis");
  ("Alex", 21, "Timis");
  ("Alex", 22, "Timis");
]

let judet lst = List.filter (fun (nume, varsta, jud) -> jud = "Timis") lst
let r = judet person;;

let judet lst j = List.filter (fun (nume, varsta, jud) -> jud = j) lst
let r = judet person "Timis";;

let varsta lst v = List.filter (fun (nume, varsta, jud) -> varsta > v) lst
let rr = varsta person 20;;
let r = judet rr "Timis";; (* let r = judet (varsta person 20) "Timis" *)

let judet j lst = List.filter (fun (nume, varsta, jud) -> jud = j) lst
let varsta v lst = List.filter (fun (nume, varsta, jud) -> varsta > v) lst

let r = person |> 
  varsta 20 |> 
  judet "Timis" |>
  List.map(fun (_, v, _) -> v) |>
  med;;

let r = person |> 
  List.filter (fun (_,_,j) -> j = "Timis") |>
  List.filter (fun (_,v,_) -> v = 20) |>
  List.map (fun (_,v,_) -> v) |>
  med;;
  
;;