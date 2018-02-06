
(*
  CS:4420 Artificial Intelligence
  Spring 2018
  The University of Iowa
   
   Instructor: Cesare Tinelli
*)

(* F# examples seen in class *)


(* Algebraic data types *)
(* aka Discriminated Unions *)

type mood = Happy | Sad | Mad


let m = Happy

let f x = 
  match x with
    Happy -> "\n:-)\n"
  | Sad -> "\n:-(\n"
  | _  -> "\n:-@\n" 


System.Console.WriteLine (f m)


type btree = 
  | Empty
  | Node of (int * btree * btree)

Empty

let e = Empty

let t3 = Node(3,e,e)

let t5 = Node(5,e,e)

let t9 = Node(9,t3,t5)

let t4 = Node(4,t9,e)


(* t4
          (4)
          / \
        (9) ()
        / \
       /   \
     (3)   (5)
     / \   / \
    () () () ()
*)


let newTree n = Node(n, Empty, Empty)




// Extracts value of root node if t is a non-empty tree
let rootValue t =
  match t with
    Node (v, _, _) -> v 
  | _ -> failwith "Empty tree"

rootValue t3

rootValue e




// Extracts left subtree if t is non-empty
let leftChild t = 
  match t with
    Node (_, t, _) -> t
  | _ -> failwith "Empty tree"

t3
leftChild t3



// Extracts right subtree if t is non-empty
let rightChild t = 
  match t with
    Node (_, _, t) -> t
  | _ -> failwith "Empty tree"




// returns true iff integer n occurs in tree t
let rec occurs n t = 
  match t with
    Empty -> false
  | Node (m, t1, t2) -> (m = n) || (occurs n t1) || (occurs n t2)

occurs 10 t4
occurs 9 t4




// "inserts" integer n in tree t so that
// all nodes to the left have a smaller or equal value
let rec insert n t = 
  match t with
    Empty -> newTree n
  | Node (m, t1, t2) ->
      if (n < m) then
        Node (m, insert n t1, t2)
      else
        Node (m, t1, insert n t2)


let s = Node (3, Empty, Node (6, Empty, Empty))
(*
     (3)
    /   \
   ()   (6)
       /   \
      ()   ()
*)

let s1 = insert 4 s

(*
     (3)
    /   \
   ()   (6)
       /   \
     (4)   ()
    /   \
   ()   ()
*)

let s2 = insert 5 s1

(*
     (3)
    /   \
   ()   (6)
       /   \
     (4)   ()
    /   \
   ()   (5)
       /   \
      ()   ()
*)


// collects all integer values in tree t into a list
let rec traverse t =
  match t with
    Empty -> []
  | Node (m,t1,t2) -> 
    let l1 = traverse t1 in
    let l2 = traverse t2 in
      l1 @ [m] @ l2

traverse s2






(* Representing object language expressions using algebraic datatypes *)

type expr = 
  | C of int
  | Op of string * expr * expr

let e1 = C 17

(*
  [17]
*)


let e2 = Op("-", C 3, C 4)

(*
      [-]
     /   \
   [3]   [4]
*)



let e3 = Op("+", Op("*", C 7, C 9), C 10)

(*
        [+]
       /   \
     [*]  [10]
    /   \
  [7]   [9]
*)


(* Evaluating expressions using recursive functions *)

let rec eval (e : expr) : int =
    match e with
    | C i -> i
    | Op("+", e1, e2) -> eval e1 + eval e2
    | Op("*", e1, e2) -> eval e1 * eval e2
    | Op("-", e1, e2) -> eval e1 - eval e2
    | Op _ -> failwith "unknown operator"

let e1v = eval e1
let e2v = eval e2
let e3v = eval e3


(* Changing the meaning of subtraction *)

let rec evalm (e : expr) : int =
    match e with
    | C i -> i
    | Op("+", e1, e2) -> evalm e1 + evalm e2
    | Op("*", e1, e2) -> evalm e1 * evalm e2
    | Op("-", e1, e2) -> 
      let res = evalm e1 - evalm e2 in
      if res < 0 then
        0 
      else 
        res 
    | Op _ -> failwith "unknown operator"


let e4v = evalm (Op("-", C 10, C 27))




(* User defined list *)

type ilist = E | L of int * ilist


let l = L(1, L(2, L(3, E)))


let head l = 
  match l with 
  | E -> failwith "list is empty!"
  | L(h, _) -> h

let tail l = 
  match l with 
  | E -> failwith "list is empty!"
  | L(_, t) -> t

let rec last l = 
  match l with 
  | E -> failwith "list is empty!"
  | L(h, E) -> h 
  | L(h, t) -> last t 



(* Parametric types *)

// 'a is a type parameter
type 'a list = E | L of 'a * 'a list

let l1 = L(1, L(2, E))


let l2 = L("a", L("b", E))


(* parametric functions *)

let id x = x

id 5

id 3.3


let toTriple x y z = (x, y, z)


// forces first two parameters to be of the same type
let toTriple (x:'a) (y:'a) (z:'b) = (x, y, z)


let equal x y = (x = y)


let distinct x y = (x <> y)



(* Lists *)

// empty list
[]

// concrete lists

[3; 5; 6; 3]

["dd"; "sdf"]

[(1,2); (1,4)]

[[3;1]; []; [4;5;6]]
  
(* ill-typed list
  
  [3; "sdf"]

*)

[6]

6::[]

6 :: []


5::(6::[])
5::6::[]
  
  (*
  
  5::6

*)

List.head [1;2;3]

List.tail [1;2;3]

let (h :: t) = [1;2;3] 


let (h :: t) = (1 :: (2 :: (3 :: []))) 


let rec len l = 
  match l with
    []     -> 0
  | _ :: t -> 1 + (len t)


len [2;5;1]

len ["a";"b"]



let rec len l = 
  match l with
    []     -> 0
  | _ :: t -> 1 + (len t)
(*
      len (1 :: 2 :: 3 :: [])
  --> 1 + len (2 :: 3 :: [])
  --> 1 + 1 + len (3 :: [])
  --> 1 + 1 + 1 + len []
  --> 1 + 1 + 1 + 0
  --> 3
*)


let rec min l =
  match l with
    [] -> failwith "Empty list!"
  | n :: [] -> n
  | n1 :: n2 :: t when n1 < n2 -> min (n1 :: t)
  | n1 :: n2 :: t -> min (n2 :: t)  


min [1;4;0;9;3]

(*
      min (1 :: 4 :: 0 :: 9 :: 3 :: [])
  --> min (1 :: 0 :: 9 :: 3 :: [])
  --> min (0 :: 9 :: 3 :: [])
  --> min (0 :: 3 :: [])
  --> min (0 :: [])
  --> 0
*)



let rec append l m =
  match l with
    [] -> m 
  | h :: t -> h :: (append t m) 

append [1;2] [2;3;4]


// append is actually predefined and has infix syntax
[1;2] @ [2;3;4]


let rec reverse = function
  [] -> []
| h :: t -> append (reverse t) [h] 

reverse [1;2;3]



// association lista are lists of type  ('a * 'b) list
let al = [("a", 3); ("c", 78); ("baf", 666); ("b", 111)]




let rec lookup al x =
    match al with 
    | [] -> failwith "key not found"
    | (k,v) :: t -> if x = k then v else lookup t x




(* Implementing finite sets in F# *)

// Sets can be represented (not very efficiently) 
// as lists with no duplicated elements

// set membership
// (mem x s) is true iff x is in s
let rec mem x s = 
  match s with
  | []      -> false
  | v :: s' -> x = v || mem x s'


// (union s1 s2) takes two lists representing sets and returns 
// a list representing their union
// (i.e., returns a list without duplicates consisting of 
// all the elements of s1 and all the elements of s2)

let rec union s1 s2 = 
  match s1 with 
  | []       -> s2
  | x :: s1' -> 
    if mem x s2 then union s1' s2  // skip x because it is already in s2
    else x :: (union s1' s2)       // add x to the union of s1' and s2


// (diff s1 s2) takes two lists representing sets and returns 
// a list representing their difference
// (i.e., returns a list without duplicates consisting of 
// all the elements of s1 that are not in s2)

let rec diff s1 s2 = 
  match s1 with 
  | []       -> []
  | x :: s1' -> 
    if mem x s2 then diff s1' s2
    else x :: (diff s1' s2)

(* Exercise

1. Define a function intersect that 
takes two lists s1 and s2 representing sets and returns 
a list representing their intersection
(i.e., returns a list with no duplicates consisting of
all the elements of s1 that occur also in s2).

2. Define a function subset that 
takes two lists s1 and s2 representing sets and returns 
true iff the first set is contained in the second
(i.e., returns true iff all the elements of s1 occur in s2).

3. Define a function setEq that 
takes two lists s1 and s2 representing sets and returns 
true iff the two sets contain exactly the same elements.

*)

