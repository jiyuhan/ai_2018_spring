(*==================================================
    CS:4420 Artificial Intelligence
    Spring 2018
    
    Homework 3
    
    Name: <your name here>

  ==================================================*)

//========
// Part 1
//========

type id = int

type interpr = id list 

type prop = True 
          | False
          | P of id
          | Not of prop
          | Or of prop * prop
          | Xor of prop * prop
          | And of prop * prop
          | Impl of prop * prop
          | Iff of prop * prop

type answer = Yes | No

(* Problem 1.1 *)

let rec meaning i p = 
  true // replace with your implementation


meaning [0;1] (Iff (P 0, P 1))
meaning [0] (Iff (P 0, P 1))


(* Problem 1.2 *)

let vars p = 
  [] // replace with your implementation



vars (Iff (Impl (P 0, P 2), And (P 1, P 2)))


(* (sublists l) returns the list of all the sublists of l
*)
let rec sublists l =
  match l with 
  | [] -> [[]]
  | h :: t -> 
    let s = sublists t in
      s @ (List.map (fun l -> h :: l) s)

sublists [1; 2; 3]

(* (satisfies p i) returns true iff 'p' is true in 'i'
*)
let satifies i p = meaning i p

(* (isValid p) returns Yes iff 'p' is a valid proposition
*)
let isValid p =
  let interpretations = sublists (vars p) in
  if List.forall (fun i -> satifies i p) interpretations then 
    Yes
  else
    No 

isValid (Iff (Or (P 1, Not (P 1)), Not False))


(* Problem 1.3 *)

let isUnsat p = 
  No // replace with your implementation
  



(* Problem 1.4 *)

let entails ps p =
  No // replace with your implementation


entails [P 1; P 2; P 3] (Or (P 2, P 5))
entails [P 1; P 2; P 3] (Or (P 8, P 5))


(* Problem 1.5 *)

let areEquiv p1 p2 = 
  No // replace with your implementation


(* Problem 1.6 *)

let rec normalize p =
  p // replace with your implementation



//========
// Part 2 
//========

(* 

*)


//========
// Part 3 
//========

type prop2 = TT
           | FF
           | V of id
           | ITE of prop2 * prop2 * prop2

(* Problem 3.1 *)

let rec convert p = 
  TT // replace with your implementation


let p = Iff (Or (P 1, Not (P 1)), Not False)

convert p


(* Problem 3.2 *)

let rec meaning2 i p = 
  false // replace with your implementation

let i = []

meaning i p = meaning2 i (convert p)


//========
// Part 4 
//========

(* Problem 4.1 *)

let threeRook = [
  // write your propositions in here
  ]


(* Problem 4.2 *)

let q = 
  True // replace with your definition


(* Problem 4.3 *)

let addConjuncts p l = List.fold (fun p1 p2 -> And (p1, p2)) p l

let findModel ps =
  let p = addConjuncts True ps in
  let rec check (i:interpr) l = 
    match l with
    | [] -> if satifies i p then Some i else None
    | v :: t ->
      let c = check i t in
      match c with
      | None -> check (v :: i) t
      | Some _ -> c 
  in
    check [] (vars p)


findModel [Not (P 2); P 0; Or (P 1, P 2)]

findModel threeRook

findModel (q :: threeRook)


(* Problem 4.3 *)

(* 

*)

