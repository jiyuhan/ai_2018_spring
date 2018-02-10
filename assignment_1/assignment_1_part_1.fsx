(* Part One *)

(* Question 1 *)

let rec sum (l: (float list)) =
    match l with
        [] -> 0.0
      | h :: t -> h + (sum t)

sum [1.4; 3.1; 3.1]

(* ================= *)

(* Question 2 *)

let rec concatAll (l: list<string>) : string =
    match l with
        [] -> ""
      | h :: t -> h + (concatAll t)

concatAll ["un"; "be"; "liev"; "a"; "ble"]

(* ================= *)

(* Question 3 *)

let rec squareAll (l: list<int>): list<int> = 
    match l with
        [] -> []
      | h :: t -> (h * h) :: (squareAll t)

squareAll [1; 2; -3; 0]

(* ================= *)

(* Question 4 *)

let rec remove (x: 'a) (l: 'a list): 'a list = 
    match l with
        [] -> []
      | h :: t when h = x -> remove x t
      | h :: t -> h :: remove x t

remove 2 [2; 1; 3; 3; 2; 1]

(* ================= *)

(* Question 5 *)

let rec replaceAll (x: 'a) (y: 'a) (l: 'a list): 'a list =
    match l with
        [] -> []
      | h :: t -> if h = x then y :: replaceAll x y t else h :: replaceAll x y t

replaceAll 2 22 [2; 1; 3; 2; 1]

(* ================= *)

(* Question 6 *)

let rec removeDuplicates (l: 'a list): 'a list = 
    match l with
        [] -> []
      | h :: t -> h :: removeDuplicates (remove h t)

removeDuplicates [4; 3; 4; 5; 5; 3; 2]

(* ================= *)

(* Question 7 *)

let rec pair (l1: 'a list) (l2: 'b list) =
    match (l1, l2) with
      ([], []) -> []
    | (h1 :: t1, h2 :: t2) -> ((h1, h2) :: (pair t1 t2))
    | (_, _) -> failwith "shouldn't exist, wrong length?"

// correct example
pair [4; 2; 5; 9] ["a"; "b"; "c"; "d"]

// incorrect example
pair [4; 2; 5; 9] ["a"; "b"; "c"; "d"; "e"]

(* ================= *)

(* Question 8 *)

let rec move (l1: 'a list) (l2: 'a list): 'a list = 
    match l1 with
        [] -> l2
      | h1 :: t1 -> move t1 (h1 :: l2)

move [1; 2; 3] [7; 8]

(* ================= *)

(* Question 9 *)

let reverse l =
    move l []

reverse [1; 2; 3; 4]

(* ================= *)

(* Question 10 *)
    
let popHead l =
    match reverse l with
        [] -> failwith "Wrong length"
      | [h] -> []
      | _ :: t -> t

let rec popHeadAndTail (l: 'a list): 'a list =
    match reverse l with
        [] -> failwith "Wrong length"
      | [h] -> [h]
      | _ :: t -> popHeadAndTail (popHead t)


let middle l =
    match popHeadAndTail l with
        [] -> failwith "Something went wrong"
      | [h] -> h
      | _ :: _ -> failwith "Something went wrong..."

// length odd
middle ['A'; 'B'; 'G'; 'D'; 'E']

// length even
middle ['A'; 'B'; 'C'; 'D'; 'E'; 'F']

middle ['1'; '2'; '3'; '4'; '5'; '4'; '7'; '8'; '9']