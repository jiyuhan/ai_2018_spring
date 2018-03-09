
(*
  CS:4420 Artificial Intelligence
  Spring 2018
  The University of Iowa
   
   Instructor: Cesare Tinelli
*)

(* F# examples seen in class *)


let f x = if x = true then true else false



(* functions as values: closures *)

let f x = x + 1

let f = fun x -> x + 1  // true syntax for declaration above

// 'f' is just an immutable variable whose value is a function from 'int' to 'int'

// fun x -> x + 1   is an "anonymous" function. It can be applied directly to an argument
(fun x -> x + 1) 5

let g = fun x -> 2 * x

// functions can be stored in a data structure
let p = (f, g)

// apply the first element of pair 'p' to 4
(fst p) 4

// functions in a list
let l = [f; g]

// apply the first element of 'l' to 8 
(List.head l) 8

// apply the ast element of 'l' to 8 
(List.last l) 8



(* Higher-order functions *)

let m = 4

let h = fun x -> x + m

// the value of 'f' is a "closure", a function that also carries
// internally a value for the global variable 'm' 


h 10

// 'make_incr' takes an integer 'n' and returns a unary function that
// computes the result of incrementing its input 'x' by 'n'
let makeIncr n = (fun x -> n + x)

// 'add6' is a unary function that returns
// the result of incrementing its input by 'n'
let add6 = makeIncr 6

add6 10



// a binary function is actually unary functions that returns another unary function
let add m n = m + n 

// the definition above is a shorthand for
let add = fun m -> (fun n -> m + n)

// this means that 'add' can be applied to just one argument
let add1 = add 1 

// 'add1' is a function that takes and integer and returns its successor
add1 5

// this syntax ...
add 1 5
// ... is the same as
(add 1) 5

// The predefined '+' operator can be partially applied too, with this syntax
let add4 = (+) 4

add4 10

// 'applyTwice' takes a unary function 'f' with same domain and range, and 
// a value 'x' in that domain, and applies 'f' twice to 'x'
let applyTwice f x = f (f x) 

applyTwice (fun x -> x + 5) 3

applyTwice (add 10) 3

let double x = x + x 

applyTwice double 5

// 'compose' takes a function 'f', a function 'g' and a value 'x' and returns
// the result of applying 'f' to the result of applying 'g' to 'x'
let compose f g x = f (g x) 


let square x = x * x 

let incSquare = compose add1 square

incSquare 3


let add2 = compose add1 add1 

add2 3



// same as 'compose' but with infix syntax and name '<*>'
let (<*>) f g x = f (g x)

(add1 <*> square) 3

((fun x -> x + 1) <*> (fun x -> x * x)) 3

// similar to 'applyTwice'
let twice f = f <*> f 

(twice add1) 3



// pipe function

let (|>) x f = f x

// the infix function '|>' allows us to write function applications in
// "dataflow" style, that is, with the argument coming before the function

4 |> square 

// this style allows us to pipe togethe several unary functions
4 |> square |> (add 7) |> double |> add1

// the pipeline above is the same as 
add1 (double (add 7 (square 4)))


type prop = True 
          | False
          | P of int
          | Not of prop
          | Or of prop * prop
          | Xor of prop * prop
          | And of prop * prop
          | Impl of prop * prop
          | Iff of prop * prop


// map combinator
// 'map' takes a unary function 'f' and a list 'l',
// applies 'f' to each element of 'l', collects the results 
// in a list and returns it
let rec map f l = 
  match l with
  |     [] -> []
  | h :: t -> (f h) :: (map f t)

map (add 1) [1; 2; 3]

map square [1; 2; 3]

map ((+) "  ") ["a"; "b"; "c"]

map (fun x -> x + x + x) ["a"; "b"; "c"]

// datatype constructors are functions too
map Not [P 1; Or (P 1, P 2); And(True, Or (P 1, P 2)); P 5]

map And [(P 1, P 2); (P 1, P 4); (P 2, P 5)]


// foldLeft combinator
// 'foldLeft' takes a binary function 'f', an initial value 'a' and 
// a list [v1; v2, ..., vn], and returns the same result as  
// f(... f(f(a,v1),v2) ..., vn)
let rec foldLeft f a l =
  match l with
  |     [] -> a
  | h :: t -> foldLeft f (f a h) t


foldLeft (+) 0 [1; 2; 3] // computes same result as  add(add(add(0,1),2),3)

foldLeft (+) 0 [1; 2; 3; 4] 

foldLeft (*) 1 [1; 2; 3; 4]

foldLeft (+) "" ["1"; "2"; "3"; "4"]

foldLeft (+) "" (map (fun x -> x + " ") ["1"; "2"; "3"; "4"])

foldLeft (+) "" ["1"; "2"; "3"; "4"]

foldLeft (@) [] [[1; 2]; [3; 4]; [5]]

foldLeft (fun x y -> And (x,y)) True [P 1; P 2; P 3; P 4]

foldLeft (fun x y -> And (x,y)) True [P 1; Impl (P 1, P 2); Or(P 7, Or (P 1, P 2))]

// foldRight combinator
// 'foldRight' takes a binary function 'f', a list [v1; v2, ..., vn], and
// an initial value 'a', and returns the same result as  
// f(v1, f(v2, ... f(vn,a) ...))
let rec foldRight f l a =
  match l with
  |     [] -> a
  | h :: t -> f h (foldRight f t a)

foldLeft (+) "" ["a"; "b"; "c"; "d"]

foldRight (fun x y -> And (x, y)) [P 1; P 2; P 3; P 4] True


// forAll combinator
// 'forAll' takes as input a predicate 'p' (ie, a unary function into 'bool')
// and a list 'l'; it returns true if 'p' is true for every element in 'l',
// and returns false otherwise
let rec forAll p l = 
  match l with
  | []     -> true
  | h :: t -> (p h) && (forAll p t)


forAll (fun x -> x = 0) [0; 1; 0]

forAll (fun x -> x = 0) [0; 0; 0]

forAll (fun x -> x > 0) [1; 20; 4]

forAll ((<=) 0) [1; 20; 4]


// filter combinator
// 'filter' takes as input a predicate 'p' and a list 'l', and
// returns in a list, in the same order, exactly the elements 
// of 'l' that satisfy 'p'

let rec filter p l = 
  match l with 
  | []     -> []
  | h :: t when (p h) -> h :: (filter p t)
  | _ :: t -> filter p t

filter (fun x -> x <> "c") ["a"; "b"; "c"; "d"; "c"]

filter (fun x -> x > 0) [1; 20; -4; 5; 0] 

// filter can be combined with othe function via piping
filter (fun x -> x > 0) [1; 20; -4; 5; 0]  |>  map square

filter (fun x -> x > 0) [1; 20; -4; 5; 0]  |>  map (square <*> square) |> forAll (fun x -> x > 10)


