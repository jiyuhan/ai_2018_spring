
(*
  CS:4420 Artificial Intelligence
  Spring 2018
  The University of Iowa
   
   Instructor: Cesare Tinelli
*)

(* F# examples seen in class *)


(* Function declarations *)

// declaring a funtion in integer input and integer output
let next (n : int) : int = n + 1

next // next is bound to a function from int to int

next(4)

next 4  // more common syntax 

next next 4  // incorrect syntax: read as ((next next) 4)

next (next 4) // correct syntax


// input and output types are usually inferred automatically
let fnext n = n + 1.0

// integer version of + is used in the absence of additional type info
let f x = x + x  

// Type constraints can be added as needed

let double (x : string) = x + x

let double x : float = x + x

let double (x : int) : int = x + x

// functions with unit return type
let print_value i = printfn "The value is %i" i


// this expression evaluates to ()
// but also has the side effect of printing 
// a message on the standard output stream
print_value 4


(* Recursive function declarations *)

let rec fac n = if n = 0 then 1 else n * fac (n - 1)

fac 7


(* Mutually recursive function declarations *)

let rec even n = if n = 0 then true else odd (n - 1)
and     odd  n = if n = 0 then false else even (n - 1)


(* Let binding visibility  *)

let a = 5

let f x = a + x

f 1

let a = 2  // *new* constant with name

// new constant shadows old one
a

// however, the old one still exists and is unchanged
// f was defined using the old a, not the new one
f 1 

// declarations can be nested
let h = (let z = 4 in z + z)
//       ^^^^^^^^^^^^^^^^^^ z is a local variable

// The scope of a let binder is the rest of the file
// unless explicitly reduced with 'in'  

let q = 3 in q + 1
//           ^^^^^ scope of q

q //  q not defined here

let x = 12              // outer x is 12, its scope starts with this line 
                        // and extends until the end of the file

let r = let x = 3 in
          x * x         // inner x is 3, its scope is only this line

x                       // outer x unchanged


// nested declarations shadow less local ones 
let k = 1 in
  (let k = 2 in  
    (let k = 3 in
      printfn "%i" k
    ); // within scope of each of three lets
    printfn "%i" k   // within scope of first two lets only
  );
  printfn "%i" k     // within scope of first let only


// function definitions can have local scope too

let h x = x + 3 in 
  h 1                // scope of identifier h

h  // error


// this implies that functions can be defined locally 
// within other functions

let f x = 
 let l_square x = x * x in
 let l_double x = x + x in
 l_square(x) - l_double(x)

f 5

l_square 3 // error



(* Pattern matching *)

let t = (3,5,2)

// left-hand sides of let can be a pattern with variables

// declares new variables x1, x2, x3 and assigns the value 3, 5, 2, respectively
let (x1,x2,x3) = t

// extracts only first two components of t, into y1 and y2 respectively
let (y1,y2,_) = t


let (y1, y2) = t // error, pattern/type mismacth


let t = ((2,3), 5, (6,2))


// deep pattern matching: extracts first element of third element of t
let (_, _, (x,_)) = t



// patterns can also occur in argument positions of function definitions

let sum (x,y) = x + y
//      ^^^^^ tuple pattern!   

// sum is a *unary* function, taking a 2-element tuple of integers as argument

sum (3,4)


// contrast with this *two* argument function
let sum2 x y = x + y

sum2 3 4       // takes two arguments

// nested patterns are possible
let psum ((x1,x2), (y1,y2)) = (x1 + y1, x2 + y2)
//       ^^^^^^^^^^^^^^^^^^ nested pattern

psum ((1,2), (3,4))

// patterns are extremely useful with the match construct:
//
//    match T with 
//      P1 -> T1 
//    | P2 -> T2
//    ...
//    | Pn -> Tn 
//
// where T, T1, ..., Tn are terms and P1, ..., Pn are patterns.
// All the Ti's must have the same type.
// Term T1 is matched against each pattern Pi in sequence, 
// stopping with the first one that matches.
// The value of the whole match is the value of the corresponding Ti
// The scope of any variable in patter Pi is just term Ti
// Each Pi -> Ti is called a _rule_

let n = 1

match n with
  0 -> "zero"   // pattern matches only 0
| 1 -> "one"    // pattern matches only 1
| _ -> "other"  // pattern matches any number


// since match expressions are indeed expressions
// the can occur in any expression position.


"the value of n is " + match n with
                         0 -> "zero"
                       | 1 -> "one"
                       | _ -> "other"



let conv n = 
  match (n * n + 1) with // matched term can be any term
      0 -> "zero"
    | 1 -> "one"
    | 2 -> "two"
    | _ -> "enough already"


// match expressions allow for more coincise and readable close

// Is is immediately clear that this function returns true
// iff both inputs are 0?
let bothZero x1 x2 = 
  if x1 <> 0 then
    false
  else if x2 = 0 then 
    true
  else
    false

// How about this one?
let bothZero x y =
  match (x,y) with
      (0,0) -> true
    | _     -> false 


// defining the xor operator
let xor x y = 
  match (x,y) with
    (false, false) -> false
  | (false, true) -> true
  | (true, false) -> true
  | (true, true) -> false

// using patterns to merge cases 
let xor x y = 
  match (x,y) with
    (false, true) -> true
  | (true, false) -> true
  | _             -> false



// the input here is a pair that can be pattern matched directly
let and p =
  match p with
    // returns the second component of p if the first component is true
    (true, y) -> y         
    // returns false if the first component of p is false
  | (false, _) -> false


// pattern variables are defined only locally in each match rule

let f p =
  match p with
    (true, y) -> y         
  | (false, _) -> not y   // error: y is undefined here


// a pattern consisting of just a variable matches anything
match 4 with
  x -> x + 1  
| y -> y + 2  // unreachable case!


// non-exhastive patterns lead to run-time errors
match 4 with
  1 -> "OK"  
| 2 -> "Also OK"

// compiler will ofter issue a warning with non-exhastive patterns 
let g x = 
  match x with
    1 -> "OK"  
  | 2 -> "Also OK"



// same shadowing rules apply for pattern variables too
let f v1 v2 = 
  match (v1, v2) with
    (5, v1) -> v1    // local v1 shadows input v1,   
  | _ -> v1  // this is the input v1

f 5 6

f 4 6


// match is also helpful in writing more readable recursive functions
  
let rec fact n = 
  if n = 0 then
    1
  else
    n * (fact (n - 1))


let rec fact n = 
  match n with
      0 -> 1
    | _ -> n * (fact (n - 1))


// alternative syntax for function declarations with pattern matching
// the argument variable is implicit and matching is done on its value 
let rec fact = function 
    0 -> 1
  | n -> n * (fact (n - 1))


let bothZero = function
    (0,0) -> true
  | _     -> false 


