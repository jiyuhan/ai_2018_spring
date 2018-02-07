(* part two *)

type 'a Tree = 
    | Empty 
    | Node of ('a * ('a Tree) * ('a Tree))

(* question 1 *)

let rec size t = 
    match t with
        Empty -> 0
      | Node (_, Empty, Empty) -> 1
      | Node (_, t1, t2) -> 1 + size t1 + size t2

(* ================= *)

(* Question 2 *)

let rec leftmost (t: 'a Tree): 'a option =
    match t with
        Empty -> None
      | Node (a, Empty, _) -> Some a
      | Node (_, t1, _) -> leftmost t1

(* ================= *)

(* Question 3 *)

let rec lreplace x t =
    match t with
        Empty -> Empty
      | Node (_, Empty, t2) -> Node (x, Empty, t2)
      | Node (a, t1, t2) -> Node (a, lreplace x t1, t2)

(* ================= *)

(* Question 4 *)

let rotateRight t =
    match t with
        Empty -> Empty
      | Node (_, Empty, Empty) -> t
      | Node (_, Empty, _) -> t
      | Node (_, _, Empty) -> t
      | Node (_, Node(_, Empty, Empty), _) -> t
      | Node (_, Node(_, _, Empty), _) -> t
      | Node (_, Node(_, Empty, _), _) -> t
      | Node (a, Node(b, t1, t2), t3) -> Node(b, t1, Node(a, t2, t3))

(* ================= *)

(* Question 5 Extra credit *)

let rec flatten t =
    match t with
        Empty -> Empty
      | Node (_, Empty, Empty) -> t
      | Node (_, Empty, _) -> t
      | Node (_, _, Empty) -> flatten t
      | Node (a, Node(b, Empty, Empty), t3) -> flatten (Node(b, Empty, Node(a, Empty, t3)))
      | Node (a, Node(b, t1, Empty), t3) -> flatten (Node(b, t1, Node(a, Empty, t3)))
      | Node (a, Node(b, Empty, t2), t3) -> flatten (Node(b, Empty, Node(a, t2, t3)))
      | Node (a, Node(b, t1, t2), t3) -> flatten (Node(b, t1, Node(a, t2, t3)))