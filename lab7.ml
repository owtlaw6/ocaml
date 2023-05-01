module Stringmap = Map.Make(String)

let e = Stringmap.empty;;

let eb = Stringmap.bindings e;;

let mymap = e 
  |> Stringmap.add "A" 1 
  |> Stringmap.add "B" 2 
  |> Stringmap.add "C" 3;;

let mymapb = Stringmap.bindings mymap;;

let oflist1 lst = List.fold_right (fun (key, value) t -> Stringmap.add key, value t) lst [];;

let rec oflist lst = match lst with
  | [] -> Stringmap.empty
  | (key, value) :: t -> Stringmap.add key value (oflist t)

let r = oflist1 [("A", 1); ("B", 2); ("C", 3)] |> Stringmap.bindings;;
let r = oflist [("A", 1); ("B", 2); ("C", 3)] |> Stringmap.bindings;;

let r = oflist [("A", 1); ("B", 4); ("C", 3); ("B", 2)];;
let ptB = Stringmap.find "B" r;;
let ptX = Stringmap.find "X" r;;
let ptXt = try Stringmap.find "X" r with Not_found -> -1;;

let findwdefault key dictionary valDefault = 
  try Stringmap.find key dictionary with 
    | Not_found -> valDefault
    | Failure(_) -> valDefault;;

let a = findwdefault "X" r (-1);;
let a = findwdefault "B" r (-1);;

(* NU FOLOSESTI ASTA LA CODUL TAU *)
let d = if (findwdefault "X" r 0) <> 0
  then print_string "Am cheie"
  else print_string "Nu am cheie";;

let d = if Stringmap.mem "X" r
  then "Da cheie"
  else "Nu cheie";;

(*fac filter folosind fold*)
let d = Stringmap.empty 
  |> Stringmap.add "A" 1
  |> Stringmap.add "B" 2
  |> Stringmap.add "C" 3
  |> Stringmap.add "D" 4
  |> Stringmap.add "E" 5;;

let f = Stringmap.filter (fun key value -> value > 2) d |> Stringmap.bindings;;
let s = Stringmap.fold (fun key value rez -> value + rez) d 0;;
let ff = Stringmap.fold (fun key value rez -> if value > 2 then Stringmap.add key value rez else rez) d Stringmap.empty
  |> Stringmap.bindings;;
let ffgeneral pred dict = 
  Stringmap.fold (fun key value rez -> if pred key value then Stringmap.add key value rez else rez) dict Stringmap.empty;;
let sol = ffgeneral (fun key value -> value > 2) d |> Stringmap.bindings;;

(*
A->x
B->y
C->z

x->H
y->T
z->G

compuse
A->H
B->T
C->G
*)

let f1 = Stringmap.empty
  |> Stringmap.add "A" "X"
  |> Stringmap.add "B" "Y"
  |> Stringmap.add "C" "Z";;
let f2 = Stringmap.empty
  |> Stringmap.add "X" "H"
  |> Stringmap.add "Y" "T"
  |> Stringmap.add "Z" "G";;

let combin d1 d2 = Stringmap.fold (fun key value rez -> Stringmap.add key (Stringmap.find value d2) rez) d1 Stringmap.empty;;
let sol = combin f1 f2 |> Stringmap.bindings;;

(*suma elem din lista cu ac ultima cifra*)
module Int = struct
  type t = int
  let compare = compare
end
module Intmap = Map.Make(Int)

let lst = [1; 11; 12; 22];;
let funct lst = List.fold_left 
  (fun r e -> 
  let key = e mod 10 in 
  try Intmap.add key ((Intmap.find key r) + e) r with Not_found -> Intmap.add key e r) 
  Intmap.empty lst;;
let r = funct lst |> Intmap.bindings;;

;;