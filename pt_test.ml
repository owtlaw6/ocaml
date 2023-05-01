(*1. Scrieți o funcție care ia o listă de asociere cu perechi de tip (șir, întreg) și 
creează un dicționar în care fiecare șir e asociat cu suma tuturor valorilor cu care e asociat în listă.*)

module Stringmap = Map.Make(String)
module Intgmap = Map.Make(Int)

let funct lst = List.fold_right(fun (k, v) r -> Stringmap.add k v r) lst Stringmap.empty;;

let r = funct [("s", 1); ("b", 4)] |> Stringmap.bindings;;

(*2. Scrieți o funcție care ia o listă de șiruri de caractere și creează un dicționar 
în care fiecare șir e asociat cu numărul aparițiilor din listă*)

let funct lst = List.fold_right(fun k r -> 
  try Stringmap.add k ((Stringmap.find k r) + 1) r with Not_found -> Stringmap.add k 1 r)
  lst Stringmap.empty;;

let r = funct ["a"; "b"; "c"; "b"; "a"; "a"; "a"] |> Stringmap.bindings;;

