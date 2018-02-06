(* part two *)

type 'a tree = Empty | Node of 'a * ('a tree) * ('a tree)

(* question 1 *)

let rec size t = 
    match t with
        Node (_, Empty, Empty) -> 0
      | Node (_, t1, t2) -> 2 + size