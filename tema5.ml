(*7*)
let split l=List.fold_right(fun (e1,e2) (r1,r2)->(e1::r1,e2::r2) )l ([],[]);;
split [(1,2);(4,3)];;

let rec combine l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (a1::l1, a2::l2) -> (a1, a2) :: combine l1 l2
  | (_, _) -> invalid_arg "List.combine";;
combine [1;2] [5;6];;

(*8*)
let partition p l =
  let rec part yes no = function
  | [] -> (List.rev yes, List.rev no)
  | x :: l -> if p x then part (x :: yes) no l else part yes (x :: no) l in
  part [] [] l;;
partition (fun x->x mod 2=0) [1;2;3;4];;

(*10*)
let rec duplicate l = match l with
|[]->[]
|e1::e2::t when e1=e2->e1:: duplicate t
|e1::t->e1::duplicate t;;
duplicate [1;1;2;3;3;4];;

(*12*)
let rec merge l1 l2 =
  match l1, l2 with
  | [], l2 -> l2
  | l1, [] -> l1
  | h1 :: t1, h2 :: t2 ->
      if h1<h2
      then h1 :: merge t1 l2
      else h2 :: merge l1 t2;;
merge  [1;2;7;8] [2;3;5;6];;

(*13*)
let desparte l=List.fold_right(fun f(h1,h2)->(f::h2,h1)) l ([],[]);;
desparte [1;2;3;4;5];;

