
(*
  CS:4420 Artificial Intelligence
  Spring 2018
  The University of Iowa
   
   Instructor: Cesare Tinelli
*)

(* F# examples seen in class *)


(* Immutable records *)

type employeeType = { 
       id : int ;     
       name : string ; 
       age : int ; 
       salary : float ; 
       dept : char
     }


let e = { id = 1098; name = "Joe Bull"; age = 22; salary = 3400.4; dept = 'S' }

e.name

e.dept

let { id = i; name = n } = e


let e1 = { e with age = 40; dept = 'A' }

e


// semicolon separator can be replaced by a new line character
let e = { id = 1098
          name = "Joe Bull"
          age = 22
          salary = 3400.4
          dept = 'S' 
         }


(* Mutable record types *)

type mut_rec = {
       mutable x : int ;
       y : int
     }

let r = { x = 5; y = 3 }

// assignment 
r.x <- 10

r

r.y <- 110



type linkedList = { value : int ; mutable next : linkedList }



(* Option Types *)

type 'a option = None | Some of 'a


let o1 = Some 0.4

let o2 = Some "aaa"

let o3 = None 

let l = []


let f x = 
  match x with
  | Some _ -> "I got something"
  | None -> "I got nothing"

f (Some 5)

f None


let g (x: int option) = 
  match x with
  | Some n -> "I got " + string n
  | None -> "I got nothing"


g (Some 5)


(* alternative syntax for parametric types *)

let l1 : int list = [1;2]

let l2 : List<int> = [1;2]


(* Immutable maps *)

let m = Map.empty

let m1 = Map.add "a" 1 m

let m3 = Map.add "b" 3 (Map.add "d" 6 m1)


let m4 = Map.empty |> Map.add "a" 1 |> Map.add "b" 3 |> Map.add "c" 0


let m5 = Map.remove "b" m4

m4


Map.find "b" m4

Map.find "b" m5

Map.tryFind "b" m5

Map.tryFind "b" m4







(* mutable variables *)

let mutable z = 4

z <- 11

// assignments are expressions that have value ()
// the value is not interesting, we evaluate assignment only for
// their side effect (changing the value of a mutable variable)

let mutable a = 1
let mutable b = 2

let x = a <- 3

x

(* expression sequences *)

// ; is an infix binary operator that takes two expressions of any type, 
//   evaluates them in order and returns the value of the second

3 ; 5            // evaluates to 5

"abc" ; (1+4)    // also evaluates to 5


// all these evaluate to 5
3; 4; 5  

(3; 4); 5

3; (4; 5)


let a = 3;4;5

a

// ; is interesting only with expression that have *side effects*
// such as assignments

// assigns 4 to a, returns 5
a <- 4; 5

// assigns 7 to a, assigns 8 to b, returns ()

a <- 7; b <- 8


a


// ; can be replaced by the end-of-line character
(a <- 7
 b <- 8
)

a
b

// ; can be replaced by end of line char
let f x = 
  if x > 0 then (
    a <- 10
    b <- 11
  ) else ()


f 4

a

b


// while loops

while (z > 0) do (printfn "%i\n" z; z <- z - 1)


// semicolon separator can be replaced by a new line character
// and indentation
while (z > 0) do
  printfn "%i\n" z
  z <- z - 1

