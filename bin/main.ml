open Arith

let () =
  let lhs = 2 in
  let rhs = 3 in
  Printf.printf "%d + %d = %d\n" lhs rhs (add lhs rhs)
