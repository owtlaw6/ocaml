(* 2. Expresii numerice
Folosind definiția tipului expresie de la curs, scrieți în ML reprezentarea pentru 
expresiile: 2 * (3 - 8) + 4 și 2 + 4 - 5 * 3 . Verificați că puteți aplica funcția de 
evaluare și obțineți rezultatul corect. *)

type expr = I of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

  let rec eval = function
  | I i -> i
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) -> eval e1 / eval e2

 let r1 = eval (Add (I 2, Sub(I 4, Mul(I 5, I 3))))
 let r2 = eval (Add (Mul(I 2, Sub(I 3, I 8)), I 4));;

(* 3. Cel mai mare divizor comun
Știind că cmmdc(a, b) = cmmdc(b, a mod b) dacă b ≠ 0, scrieți o funcție 
recursivă pentru cel mai mare divizor comun. Care e cazul de bază ?*)

let rec cmmdc a b = 
  if b == 0 then a 
  else if a == 0 then b 
  else cmmdc b (a mod b);;

cmmdc 64 22;;

(* 6. Resturi modulo p
În matematică știm că dacă p e un număr prim, și a nu se divide cu p, atunci șirul 
a, a2, a3, ... va ajunge la 1, luând numerele modulo p (adică resturile la împărțirea cu p).
De exemplu, fie p = 7 și a = 4. Atunci a2 = 16 ≡ 2 (mod 7), și a3 = a2 * a ≡ 2 * 4 ≡ 1 (mod 7).
(Se spune că mulțimea resturilor nenule modulo p prim formează un grup multiplicativ.)
Scrieți o funcție care ia ca parametru un număr întreg a și un număr p (presupus prim) și 
returnează cea mai mică putere n pentru care an ≡ 1 mod p (sau returnează 0 dacă a se divide cu p).
Indicație: scrieți o funcție auxiliară care mai are ca parametri și exponentul k respectiv 
valoarea ak (mod p), și care se apelează recursiv până când ak ≡ 1 (mod p).*)

let rec func p a sol contor = 
  if sol == 1 then contor
  else func p a ((sol * a) mod p) (contor + 1);;
(*#trace func;;*)
func 7 4 4 1;;

