let comp f g x = f (g x)

let h x = x + 1
let t x = x + 2

let ht x = h (t x)

let r0 = comp h t 0

let m = min 0
let mm = m 1
let mm1 = m (-1)

let ht = comp h t 

;;

let op3 op a b c = op (op a b) c  
let min3 = op3 min 
let max3 = op3 max 

(*let min3 a b c = min (min a b) c
let max3 a b c = max (max a b) c*)

let add3 a b c = (a +  b) + c 
let mult3 a b c = (a *  b) * c 

let add3 = op3 ( + )
let mult3 = op3 ( * )

let r = op3 ( + ) 1 2 3
let q = add3 1 2 3

;;

let f x = if x == 1 then "unu"
  else if x == 2 then "doi"
    else if x == 3 then "trei"
      else if x == 4 then "patru"
        else if x == 5 then "cinci"
          else if x == 6 then "sase"
            else if x == 7 then "sapte"
              else if x == 8 then "opt"
                else if x == 9 then "noua"
                  else "Dunno!"

let cifra c = match c with
 | 0 -> Some ("zero")
 | 1 -> Some ("unu")
 | 2 -> Some ("doi")
 | 3 -> Some ("trei")
 | 4 -> Some ("patru")
 | 5 -> Some ("cinci")
 | 6 -> Some ("sase")
 | 7 -> Some ("sapte")
 | 8 -> Some ("opt")
 | 9 -> Some ("noua")
 (* | _ -> "Dunno!" *)
 | _ -> None

let r = cifra 1
let nu = f 5 

let printCifra c = match (cifra c) with
  | None -> print_string "Nu e ok"
  | Some ("unu") -> print_string "este de bine"
  | Some ( _ ) -> print_string "toate" 
  | Some (s) when String.length s = 3 -> print_string "sth new"

  let () = print_newline ()
let r = printCifra 9
let () = print_newline ()
let x = printCifra 1
let () = print_newline ()
let a = printCifra 3
let () = print_newline ()

;;

let delta a b c = b *. b -. 4. *. a *. c
let gr2 a b c = 
  if delta a b c >= 0. then 
    if delta a b c = 0.
      then "o solutie"
      else "2 solutii"
  else "fara solutii"

let gr2v a b c = 
  let vDelta  = delta a b c in 
  if delta a b c >= 0. then 
    let sqDelta = sqrt vDelta in
    if delta a b c = 0.
      then "o solutie"
      else "2 solutii"
  else "fara solutii"

  let r = (let x = 2 * 2 in x * x)

  ;;