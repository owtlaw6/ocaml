type okerror = 
  | Ok of (int)
  | Error of (string)
(*type treeset = *)

let ok = Ok(1)
let err = Error("ceva nu a fost ok")

let fn errOk = match errOk with
  | Ok value -> Printf.printf "valoarea e %d" value
  | Error err -> Printf.printf "Err %s" err

let r = fn ok;;
let r = fn err;;

type treeset = 
  | Nil 
  | T of (treeset * int * treeset)
  
let arb = T (Nil, 1, Nil);;
let arb = T (T (Nil, 3, Nil), 1, T (Nil, 2, Nil));;

let arb = T(
  T (T (Nil, 1, Nil), 2, T (Nil, 3, Nil)),
  4,
  T (T (Nil, 5, Nil), 6, T (Nil, 7, Nil))
  );;

let rec printtree tree = match tree with
  | Nil -> Printf.printf ""
  | T(left, value, right) ->
    Printf.printf "%d " value;  (*preordine = rad stg dr*)
    printtree left;             (*inordine = stg rad dr*)
    printtree right             (*postordine = stg dr rad*)
;;
let () = printtree arb;;
let () = print_newline ();;

let rec preordine tree = match tree with
| Nil -> []
| T(left, value, right) ->
  (value::preordine left) @ (preordine right);            
;;
let r = preordine arb;;

let preordine1 tree = 
  let rec aux tree parc = match tree with
    | Nil -> parc
    | T(left, value, right) -> 
      let pargwithvalue = value :: parc in
      let parcurgereaculeftsivalue = aux left pargwithvalue in
      let parcurgereacurightsivalue = aux right parcurgereaculeftsivalue
      in parcurgereacurightsivalue
    in aux tree [];;
;;
let r = preordine1 arb;;

let preordine2 tree = 
  let rec aux tree parc = match tree with
    | Nil -> parc
    | T(left, value, right) -> 
      let prv = aux right parc in
      let plv = aux left prv in
      let pvr = value::plv
      in pvr
    in aux tree [];;
;;
let r = preordine2 arb;;

let inordine tree = 
  let rec aux tree parc = match tree with
    | Nil -> parc
    | T(left, value, right) -> 
      let prv = aux right parc in
      let pvr = value::prv in
      let plv = aux left pvr
      in plv
    in aux tree [];;
;;
let r = inordine arb;;

let rec add newvalue tree = match tree with 
  | Nil -> T(Nil, newvalue, Nil)
  | T(left, value, right) ->
    if value = newvalue then T(left, value, right)
    else if value < newvalue then T(left, value, (add newvalue right))
    else T((add newvalue left), value, right)

let arb2 = Nil 
  |> add 4
  |> add 2
  |> add 1
  |> add 3
  |> add 6
  |> add 5
  |> add 7

  let arb2 = Nil 
  |> add 1
  |> add 2
  |> add 3
  |> add 4
  |> add 5
  |> add 6
  |> add 7



;;

