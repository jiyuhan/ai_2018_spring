(*
  CS:4420 Artificial Intelligence
  Spring 2018
  The University of Iowa
   
   Instructor: Cesare Tinelli
*)

(* F# examples seen in class *)

// This is a single line comment 

(* this is a
   multi-line 
   comment 
*)


(* Copy declarations from this file into fharpi, the F# interactive top-level *)

(* If you are using Visual Studio code, press Alt and Enter together 
   to send the currently selected text directly to the F# top-level
   (or the line where the cursor is if no text is selected)
 *)


(* int values *)

3 + 5

4 - 2


(* float values *)

3.7

// + operators is overloaded
1.4 + 3.0


// no implicit conversions!
1.4 + 3   // ill-typed


(* string values *)
"hi"

"hi" + " there"  // + is overloaded

"foo" + 3 // ill-typed

"aaa".Length  // OO notation for length function application


(* bool values *)

true

not true

true && false

true || false

4 = 5   // comparison operator, not assignment!

4 < 5


(* Variable binding *)

let n = 3 // declares symbolic constant n as a synonym for 3

n // evaluates to 3

n = 4  // Boolen expression, not assignment!

let m = 4 + n


(* Symbolic constants (immutable variables and references (mutable variables) *)

let v = 3   // declares symbolic constant v as a synonym of 3

v

let r = ref 3   // declares a reference r

r

!r // the value of a reference is extracted by !

r.contents // this is the same as !r

r := 4  // references are mutable

!r

r := !r + 1


v := 4  // error, constants are immutable

(* expressions with side effects *)

// assignments are also expressions ... 
r := 5
let w = (r := 0)

// ... of type unit


(* Type ascription *)

let c : string = 55  // requires c to be of type string

let c : float = 7 // type mismatch error, no implicit upcast

// any term can be annotated with a type constraint
(4 : int)  

(((4:int) = 1) : bool)

// type constraints are useful when type inference cannot 
// be done fully automatically


(* logical operators, conditional expressions *)

if 3 < 4 then "yes" else "no"

// The if construct is an expression.
// It can be seen as a mixfix operator with three arguments: B, T1 and T2
//
//   if B then T1 else T2
//
// B must be of type bool. T1 and T2 must be of the same type.
// The type of the whole if is the type of T1 and T2

if 3 < 4 then "yes" else 4 // ill-typed

let c = 5

if (c > 0) then 10 else 20

// if expressions can go everywhere an expression of can go
(if (c > 0) then 1 else 0) + 7

let r1 = (if 3 < 4 then "yes" else "no")

r1

let r2 = (if 3 < 4 then "yes" else "no") + ", sir"

r2

(* tuple types *)

(1,3)

(3,4,5)

(3,"pp",4.3)


// note the different types of this tuples
let t1 = (1,2,3)
let t2 = ((1,2),3)
let t3 = (1,(2,3))

t1 = t2    // ill-typed, = requires its arguments to have the same type
t2 = t3    // ill-typed, = requires its arguments to have the same type



// the empty tuple is a value of type unit
()

// () is the *only* value of unit
// it is used for instance for functions 
// that are called only for their side effect

let u = printf "AI"

// expressions of type unit can be combined with ;
let u = () ; ()
// the results is also of type unit

// they allow for sequencial evaluation of expressions with side effect
let u = printf "Humpty"; printf " "; printf "Dumpty" 


