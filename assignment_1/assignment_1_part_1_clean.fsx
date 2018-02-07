(* Part One *)

(* Question 1 *)

let rec sum (l: (float list)) =
    match l with
        [] -> 0.0
      | h :: t -> h + (sum t)

(* ================= *)

(* Question 2 *)

let rec concatAll (l: list<string>) : string =
    match l with
        [] -> ""
      | h :: t -> h + (concatAll t)

(* ================= *)

(* Question 3 *)

let rec squareAll (l: list<int>): list<int> = 
    match l with
        [] -> []
      | h :: t -> (h * h) :: (squareAll t)

(* ================= *)

(* Question 4 *)

let rec remove (x: 'a) (l: 'a list): 'a list = 
    match l with
        [] -> []
      | h :: t -> if h = x then remove x t else h :: remove x t

(* ================= *)

(* Question 5 *)

let rec replaceAll (x: 'a) (y: 'a) (l: 'a list): 'a list =
    match l with
        [] -> []
      | h :: t -> if h = x then y :: replaceAll x y t else h :: replaceAll x y t

(* ================= *)

(* Question 6 *)

let rec removeDuplicates (l: 'a list): 'a list = 
    match l with
        [] -> []
      | h :: t -> h :: removeDuplicates (remove h t)

(* ================= *)

(* Question 7 *)

let rec pair (l1: 'a list) (l2: 'b list) =
    match (l1, l2) with
      ([], []) -> []
    | (h1 :: t1, h2 :: t2) -> ((h1, h2) :: (pair t1 t2))
    | (_, _) -> failwith "shouldn't exist, wrong length?"

(* ================= *)

(* Question 8 *)

let rec move (l1: 'a list) (l2: 'a list): 'a list = 
    match l1 with
        [] -> l2
      | h1 :: t1 -> move t1 (h1 :: l2)

(* ================= *)

(* Question 9 *)

let reverse l =
    move l []

(* ================= *)

(* Question 10 *)
    
let rec helperMiddle l1 l2 =
    match (l1, l2) with
        (h1 :: t1, h2 :: t2) -> if h1 = h2 then h1 else helperMiddle t1 t2
      | (_, _) -> failwith "This should never be executed. If did, you probably have an even length of list."


let middle l =
    helperMiddle l (reverse l)