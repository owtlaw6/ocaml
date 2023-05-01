let a = 5
let b = 9
let c = 2

let minimul = if a < b & a < c then a 
  else if b < a & b < c then b
    else c

let maximul = if a > b & a > c then a 
  else if b > a & b > c then b
    else c

let minim a b c = min a (min b c)

let maxim a b c = max a (max b c)

let x = minim 10 3 5
let y = maxim 3 10 8

let z = minim 0.2 5.3 8.6
let w = minim "abc" "bca" "cab"

;;