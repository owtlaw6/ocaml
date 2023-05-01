(* RANDUL 1 *)

(*implement fctia elements de la multimi*)

module Int = struct
  type t = int
  let compare = compare
end
module Iset = Set.Make(Int)

let myset = Iset.empty 
  |> Iset.add 1 
  |> Iset.add 3 
  |> Iset.add 2 
  |> Iset.add 2 
let elems mult = Iset.fold(fun e r -> e::r) mult [];;
let r = elems myset;;

(*o fct care scoate elementele duplicate din lista*)

let elimin lst = List.fold_left (fun e r -> Iset.add r e) Iset.empty lst;;
let r = elimin [1;2;3;4;1;2];;
let rr = elems r;;

(*problema 3*)
(*numara cate pers din fiecare tara sunt*)

module Smap = Map.Make(String)

let people = [
  (* Name, Country, Age *)
  ("John", "RO", 21);
  ("Alex", "UK", 21);
  ("Alex", "US", 21);
  
  ("John", "RO", 25);
  ("Dominik", "UK", 23);
  ("Raluca", "RO", 24)
]

let countby lst = List.fold_left (fun (k, v, e) r -> 
  try Smap.add k (Smap.find k r + 1) r
  with Not_found -> Smap.add k e
  ) Iset.empty lst;;

let r = countby people;;

;;