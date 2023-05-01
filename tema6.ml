
module Int=struct
type t=int
let compare=compare
end
module IS=Set.Make(Int);;
module S=Set.Make(String);;
(*module LS=Set.Make(List);;*)
open Printf;;
(*1. Scrieți o funcție care ia ca parametru o mulțime de șiruri de caractere și o tipărește, 
folosind iteratorul S.iter pentru a parcurge elementele.
 Afișați mulțimea pe o linie, între acolade { } și cu virgulă între elemente. 
 Puteți folosi print_string, print_char, print_newline (vezi modulul implicit deschis Pervasives) sau Printf.printf.
*)
let mt=S.add "abc"(S.add "bcd"(S.add "cde" (S.add "def" S.empty)));;
S.elements mt;;
let tipareste s = 
  printf"{ ";
  S.iter (fun x->printf "%s " x) mt;
  printf"}";;
  tipareste mt;;

(*4. Implementați funcția standard partition care ia ca parametri o funcție booleană f și o mulțime s 
și returnează o pereche de mulțimi, cu elementele din s care satisfac, respectiv nu satisfac funcția f.*)
let m2=IS.add 1(IS.add 2(IS.add 3 (IS.add 4 IS.empty)));;
IS.elements m2;;
let m3=IS.add 5(IS.add 6(IS.add 3 (IS.add 4 IS.empty)));;
IS.elements m3;;
(IS.partition (fun x->x!=2) m2);;


(*5. Scrieți o funcție care ia o mulțime de mulțimi (de exemplu, de șiruri),
 și returnează reuniunea (variantă: intersectia) mulțimilor.)*)
IS.elements (IS.union m2 m3);;
IS.elements (IS.inter m2 m3);;
