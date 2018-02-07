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
      | h :: t -> if h = x then remove x t else h :: remove x t

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
(*
let rec pair (l1: 'a list) (l2: 'b list): ('a * 'b list) =
    match (l1, l2) with
      ([], []) -> 
    | (h1 :: t1, h2 :: t2) -> ((h1, h2) (pair t1 t2))
    | (_, _) -> failwith "shouldn't exist"
*)
// pair [4; 2; 5; 9] ["a"; "b"; "c"; "d"]

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
    
let rec helper_middle l1 l2 =
    match (l1, l2) with
        (h1 :: t1, h2 :: t2) -> if h1 = h2 then h1 else helper_middle t1 t2
      | (_, _) -> failwith "This should never be executed"


let middle l =
    helper_middle l (reverse l)

middle ['A'; 'B'; 'C'; 'D'; 'E']