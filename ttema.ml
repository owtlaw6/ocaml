(* 2. Asemănător cu funcția fromto (din notițele de curs) care generează lista numerelor întregi 
dintr-un interval dat, scrieți o funcție care creează lista tuturor întregilor dintr-un interval 
dat, divizibili cu o valoare dată d.
Indicație: Găsiți cel mai mare număr divizibil din interval, și continuați pas cu pas. *)

let tolist a b d =
  let rec f x l =
   if x == a then l 
   else if x mod d == 0 then f (x - 1) (x :: l)
   else f (x - 1) l
  in f b [];;
let r = tolist 1 10 3;;

(* 3. a) Implementați funcția List.nth care returnează al n-lea element dintr-o listă.
Observați întâi în interpretor comportamentul funcției standard pentru valori ale lui n invalide 
(negative) sau prea mari (mai mari decât lungimea listei). Puteți produce excepția Invalid_argument 
mesaj apelând funcția invalid_arg mesaj, și excepția Failure mesaj apelând funcția failwith mesaj.

b) Implementați o funcție firstn care returnează o listă cu primele n elemente dintr-o listă dată. *)

let listnth lst x = List.nth lst x;;
let r = listnth [1;2;3;4;5;6] 6;;
let r = listnth [1;2;3;4;5;6] 5;;
let r = listnth [1;2;3;4;5;6] 4;;
let r = listnth [1;2;3;4;5;6] 0;;

let firstnth lst n = 
  let rec f i l = match l with
    | h :: t -> if i >= n then ( f (i + 1) t ) else h :: ( f (i + 1) t )
    | [] -> []
  in f 0 lst;;
let r = firstnth [1;2;3;4;5] 4;;

(* 4. Folosind funcțiile din modulul Random, scrieți o funcție care construiește o listă de n întregi 
aleatori, fiecare între 0 și b - 1. *)

let generarelista n b = 
  let rec f i lst =
    if i <= n then f (i + 1) ( (Random.int b) :: lst)
    else lst
  in f 1 [];;
let r = generarelista 5 9;;

(*8. Implementați funcția List.partition care ia ca parametru o funcție cu valori boolene și o 
listă și returnează o pereche de liste, cu elementele care satisfac, respectiv nu satisfac 
funcția f.
# List.partition (fun x -> x >= 5) [4;6;7;5;4;8;9] ;;
- : int list * int list = ([6; 7; 5; 8; 9], [4; 4])
Puteți să o scrieți cu una din prelucrările standard? Va fi final recursivă sau nu ?
Indicație: la fiecare pas, elementul curent se adaugă la una din listele din perechea-rezultat. *)

(*let listpart fn lst =
  let rec f l1 l2 = function
  | [] -> (l1, l2)
  | x :: lst -> if fn x then f (x :: l1) l2 lst else f l1 (x :: l2) lst in
  f [] [] lst;;
listpart (fun x -> x >= 5 ) [4;6;7;5;4;8;9];;*)

let listpart fn lst = List.partition fn lst;;
let r = listpart (fun x -> x >= 5) [4;6;7;5;4;8;9];;

(* 13. Scrieți o funcție care desparte o listă în două liste a căror lungime diferă cu cel mult 1, 
punând alternativ câte un element în fiecare din liste. (Funcția va returna o pereche de liste). *)

let desparte lst = List.fold_right (fun f (h1, h2) -> (f :: h2, h1)) lst ([], []);;
desparte [1;2;3;4;5];;

let listsparg lst = 
  let rec f (l1, l2) i l = match l with
    | h :: l -> if i mod 2 == 1 then f (h :: l1, l2) (i + 1) l else f (l1, h :: l2) (i + 1) l
    | [] -> (List.rev l1, List.rev l2)
  in f ([], []) 1 lst;;
let r = listsparg [4;6;7;5;4;8;9];;

