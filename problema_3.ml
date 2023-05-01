(*let an = 1301*)

let verificare an = if an mod 400 == 0 then true
  else if an mod 4 == 0 && an mod 100 != 0 then true
    else false

let x = verificare 1301

;;