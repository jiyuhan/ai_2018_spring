(*==================================================
    CS:4420 Artificial Intelligence
    Spring 2018
    
    Homework 3
    
    Name: Thomas Han

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
let XOR a b =
  match a, b with
  | true, true -> false
  | true, false -> true
  | false, true -> true
  | false, false -> false
let NAND a b = not (a && b)
let NOR a b = not (a || b)
let XNOR a b = not (XOR a b)

let containsProposition p i = List.exists (fun elem -> elem = p) i
let rec meaning (i:interpr) (p:prop):bool = 
  match (i, p) with
      (_, True) -> true
    | (_, False) -> false
    | (i, (P pid)) -> containsProposition pid i
    | (i, (Not p)) -> not (meaning i p)
    | (i, (Or (p1, p2))) -> meaning i p1 || meaning i p2
    | (i, (Xor (p1, p2))) -> XOR (meaning i p1) (meaning i p2)
    | (i, (And (p1, p2))) -> meaning i p1 && meaning i p2
    | (i, (Impl (p1, p2))) -> meaning i (Or ((Not p1), p2))
    | (i, (Iff (p1, p2))) -> XNOR (meaning i p1) (meaning i p2)


meaning [0;1] (Iff (P 0, P 1))
meaning [0] (Iff (P 0, P 1))

(* Problem 1.2 *)
let vars p = 
  let rec checkVar (p:prop) (i:interpr): interpr =
    match (p, i) with
        (True, i) -> i
      | (False, i) -> i
      | ((P pid), _) -> [pid]
      | (Not p, i) -> checkVar p i
      | (Or (p1, p2), i) -> i @ (checkVar p1 i) @ (checkVar p2 i)
      | (Xor (p1, p2), i) -> i @ (checkVar p1 i) @ (checkVar p2 i)
      | (And (p1, p2), i) -> i @ (checkVar p1 i) @ (checkVar p2 i)
      | (Impl (p1, p2), i) -> i @ (checkVar p1 i) @ (checkVar p2 i)
      | (Iff (p1, p2), i) -> i @ (checkVar p1 i) @ (checkVar p2 i)
  in
  List.distinct (checkVar p [])

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
  // lists all possible compositions
  let interpretations = sublists (vars p) in
  // checks
  if List.forall (fun i -> satifies i p) interpretations then 
    Yes
  else
    No 

isValid (Iff (Or (P 1, Not (P 1)), Not False))


(* Problem 1.3 *)

let isUnsat p = 
  // lists all possible compositions
  let interpretations = sublists (vars p) in
  // checks
  if List.forall (fun i -> (not (satifies i p))) interpretations then 
    Yes
  else
    No 




(* Problem 1.4 *)

let entails ps p =
  // checks
  if List.exists (fun ps_each -> satifies (vars ps_each) p) ps then 
    Yes
  else
    No 


entails [P 1; P 2; P 3] (Or (P 2, P 5))
entails [P 1; P 2; P 3] (Or (P 8, P 5))


(* Problem 1.5 *)
let ynToTf arg = 
  match arg with
    Yes -> true
  | No -> false
let areEquiv p1 p2 = 
  if (ynToTf (entails [p1] p2) && ynToTf (entails [p2] p1)) then Yes
  else No

(* tester *)
areEquiv (Or (P 3, P 4)) (P 3)

(* Problem 1.6 *)

let rec normalize p =
  match p with
    True -> True
  | False -> Not True
  | (P pid) -> P pid
  | (Not p) -> Not (normalize p)
  | (Or (p1, p2)) -> (Or (normalize p1 , normalize p2))
  | (Xor (p1, p2)) -> normalize (Not (Iff (p1, p2)))
  | (And (p1, p2)) -> Not (Or (Not (normalize p1), Not (normalize p2)))
  | (Impl (p1, p2)) -> Or (Not (normalize p1), normalize p2)
  | (Iff (p1, p2)) -> normalize (And (Impl (p1, p2), Impl (p2, p1)))

(* tester *)
areEquiv (normalize (Iff ((P 1), (P 2)))) (Iff ((P 1), (P 2)))

//========
// Part 2 
//========

(* 
  In order to prove a rule's soundness,
  one has to prove all of its rules are
  sound as well.

  However, it would be very easy to
  implement DPLL using the functions we
  have written so far.

  I won't go into details of how to 
  implement the function because the
  pseudocode is written on slides of
  "Propositional Logic" p.47 and p.48

  To implement DPLL, I will use our 
  normalize() function to get the CNF
  presentation of our input. Then we
  should use vars() to get all the symbols
  from our input. At the end, it will call
  function dpll(). That would be all for
  dpll_satisfiable().

  In "rec dpll()", the functions we will be using:
  (in the order below)
    satisfies()
    isUnsat()
    findPureSymbol() ->>>>>> NEEDS TO BE IMPLEMENTED
    findUnitClause() ->>>>>> NEEDS TO BE IMPLEMENTED
    first()          ->>>>>> NEEDS TO BE IMPLEMENTED
    rest()           ->>>>>> NEEDS TO BE IMPLEMENTED

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
  match p with
      True -> TT
    | False -> FF
    | P pid -> V pid
    | Not p1 -> ITE (convert p1, FF, TT) 
    | Or (p1 ,p2) -> ITE (convert p1, TT, convert p2)
    | (Xor (p1, p2)) -> ITE (convert p1, convert (Not p2), convert p2)
    | (And (p1, p2)) -> ITE (convert p1, convert p2, FF)
    | (Impl (p1, p2)) -> ITE (convert (Not p1), TT, convert p2)
    | (Iff (p1, p2)) -> ITE (convert (Impl (p1, p2)), convert (Impl (p2, p1)), FF)

let p = Iff (Or (P 1, Not (P 1)), Not False)

convert p


(* Problem 3.2 *)

let mean p = 
  match p with
    true -> TT
  | false -> FF

let rec meaning2 i p = 
  match (i, p) with
    (_, TT) -> true
  | (_, FF) -> false
  | (i, V id) -> List.contains id i
  | (i, ITE (TT, p2, _)) -> meaning2 i p2
  | (i, ITE (FF, _, p3)) -> meaning2 i p3
  | (i, ITE (p1, p2, p3)) -> meaning2 i (ITE (mean (meaning2 i p1), p2, p3))

let i = []

meaning i p = meaning2 i (convert p)


//========
// Part 4 
//========

(* Problem 4.1 *)

let threeRook = [ (Xor (P 11, (Xor (P 12, P 13)))); // the first row can only be placed with one rook.
                  (Xor (P 11, (Xor (P 21, P 31)))); // the first col can only be placed with one rook.

                  (Xor (P 21, (Xor (P 22, P 23)))); // the second row can only be placed with one rook.
                  (Xor (P 12, (Xor (P 22, P 32)))); // the second col can only be placed with one rook.

                  (Xor (P 31, (Xor (P 32, P 33)))); // the last row can only be placed with one rook.
                  (Xor (P 13, (Xor (P 23, P 33)))); // the second col can only be placed with one rook.
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
let p = addConjuncts True threeRook
vars p
findModel (q :: threeRook)


(* Problem 4.3 *)

(* 
  This function takes in the list, and it calls the addConjuncts to make all the conditions in the list true simutaneously by connecting them with ANDs.
  Then it calls another helper function to get all the available variables for it to try.

  So we have: A prove method to test if each input works, and all the available inputs in a list.
  Then it tries each and see if it satisfies the propositions, if it does, add to the list, if not, then don't add it to the list.
  If there is already something in the list, then it will take the tail and check.

  It uses a linear search strategy.
*)

