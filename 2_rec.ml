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
 let r2 = eval (Add(Mul(I 2, Sub(I 3, I 8)), I 4));;