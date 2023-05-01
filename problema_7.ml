(* Exercițiu 7: tarife cu bonus
În New York, o călătorie cu transportul în comun costă $2.75. La încărcarea cardului de 
transport se acceptă doar sume în multipli de 5 cenți. Pentru încărcarea cu valoarea a cel 
puțin două călătorii se oferă un bonus de 5%, rotunjit la cent. Scrieți o funcție care 
calculează și returnează suma minimă care trebuie incărcată pentru N călătorii, și afișează 
restul care rămâne pe card. *)

open Printf

let f n = 
  if n = 1. then (printf "restul este 0\n"; 2.75)
  else if n = 2. then (printf "restul este 0\n"; 5.5)
    else (
      printf "restul este: %.2f\n" (0.05 *. ceil (52.38 *. n) -. 0.05 *. 52.38 *. n);
      ceil (52.38 *. n) *. 0.05 
    )

(* let suma = f 5. *) (*rezultatul are mai mult de 2 zecimale*)
let suma = ( float_of_int (int_of_float (f 4. *. 100.) ) /. 100.)

;;