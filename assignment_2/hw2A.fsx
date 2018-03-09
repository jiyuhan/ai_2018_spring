(*==================================================
    CS:4420 Artificial Intelligence
    Spring 2018
    
    Homework 2 -- Part A
    
    Name:Thomas Han
  ==================================================*)

(** Debugging aid **)

// set this flag to true when debugging your code
let mutable Debug = false

// debugging
Debug <- true

// (require p f m) silently succeeds if the global flag 'Debug' 
// is false or the input 'p' is true.
// Otherwise, it fails and returns an error message constructed
// with strings 'f' and 's'.
// 'f' is meant to contain the name of the function calling 'require'
// while 's' contains the actual error message.
let require p f m =
  if not Debug || p then 
    ()
  else
    failwith (f + ": " + m)


//--------------------
// Part A -- N-puzzle
//--------------------

(** Board **)

// A position on the puzzle board is encoded just as a pair of integers
// Each coordinate ranges from 1 to n, where n is the square root of N+1.
// Position (1,1) is the top-leftmost position in the board.
type pos = int * int

// a puzzle tile is encoded just as an integers
type tile = int


// A board is encoded as an immutable record where the cells are stored
// in an immutable map from position to tiles  
// The 'board' type is equipped with several "member functions" (ie, methdods)
type board = { size: int; tiles: Map<pos,tile> }
with 
  // method 'Value' returns the values at position 'p' in board 'b', returning 0
  // if that position is unoccupied (ie, it is position of the empty tile)
  member b.Value (p: pos) =
    match Map.tryFind p b.tiles with
    | None -> 0
    | Some v -> v  
  // method 'Swap' returns a new board identical to this except that
  // the values at position 'p1' and 'p2' are swapped
  member b.Swap (p1: pos) (p2: pos) = 
    // check that the input is legal
    let inbounds (i, j) = 0 < i && i <= b.size && 0 < j && j <= b.size in
    let _ = require (inbounds p1) "board.Sqwap" "first position is out of bounds" in
    let _ = require (inbounds p1) "board.Sqwap" "second position is out of bounds" in
    //
    let t1 = Map.tryFind p1 b.tiles in
    let t2 = Map.tryFind p2 b.tiles in
    let tiles' =
      match (t1, t2) with
      | (Some v1, Some v2) -> b.tiles |> Map.add p1 v2 |> Map.add p2 v1
      | (Some v1, None)    -> b.tiles |> Map.remove p1 |> Map.add p2 v1
      | (None, Some v2)    -> b.tiles |> Map.add p1 v2 |> Map.remove p2
      | _                  -> b.tiles
    in
      { size = b.size; tiles = tiles' }
  // method 'ToString' converts the board to a textual representation
  member b.ToString =
    let cellToString tiles r c = 
      match Map.tryFind (r,c) tiles with
      | None -> "  "
      | Some (n:tile) -> " " + string n
    in
    let rec rowToString tiles r c = 
      if c <= 0 then ""
      else (rowToString tiles r (c - 1)) + (cellToString tiles r c)
    in
    let rec rowsToString tiles r c = 
      if r <= 0 then ""
      else (rowsToString tiles (r - 1) c) +
           " |" + (rowToString tiles r c) + " |\n"
    in
    let bar =  " +" + (String.replicate b.size "--") + "-+\n" 
    in
      "\n" + bar + (rowsToString b.tiles b.size b.size) + bar
  // method 'Print' prints the board to the standard output channel
  member b.Print = printf "%s" b.ToString

(* Note: 
     A value b of type board represents a legal puzzle board in a problem 
     of size n iff 
     1) n = b.size > 0 
     2) b.tiles uniquely maps *each* non-empty tile in the n x n board 
        to a value in {1, ..., n}
     3) the position of the empty cell has no associated value in b.tiles
*)
 
 // sample map for a board
let ts = 
  Map.empty 
  |> Map.add (1,1) 2
  |> Map.add (1,2) 8
  |> Map.add (1,3) 3
  |> Map.add (2,1) 1
  |> Map.add (2,2) 6
  |> Map.add (2,3) 4
  |> Map.add (3,1) 7
  |> Map.add (3,2) 5

// sample board
let b1 = { size = 3; tiles = ts } 

b1.Print

 (* Boards are converted to strings that print like this:
  +-------+
  | 2 8 3 |
  | 1 6 4 |
  | 7 5   |
  +-------+
 *)

// new board generated from b
let b2 = b1.Swap (2,3) (3,3)

b2.Print 


(** State **)

// A problem state is encoded as a record with a board and the position
// of the empty cell in the board
type state = {board: board; emptyPos: pos}
with 
  member s.ToString = s.board.ToString  + " " + (string s.emptyPos) + "\n"  
  member b.Print = printf "%s" b.ToString

 (* States are converted to strings that pring like this:
     +-------+
     | 1 3 2 |
     | 4   7 |
     | 6 8 5 |
     +-------+
     (2,2)
  *)

  (* Note: 
     A value s of type state represents a legal problem state iff 
     s.board is a legal board and (Map.tryFind s.board.tiles s.emptyPos) is None 
  *)

// sample state using previous board b 
let s1 = {board = b1; emptyPos = (3,3)}

s1.Print

// sample goal state
let goalState = {
  emptyPos = (3, 3);
  board = { size = 3;
            tiles = Map.empty 
                    |> Map.add (1,1) 1
                    |> Map.add (1,2) 2
                    |> Map.add (1,3) 3
                    |> Map.add (2,1) 4
                    |> Map.add (2,2) 5
                    |> Map.add (2,3) 6
                    |> Map.add (3,1) 7
                    |> Map.add (3,2) 8
         }
}

goalState.Print


(** Operators **)

//-------------
// Problem A.1
//-------------
//
// In the four auxiliary functions of method 'Apply' below
// replace 'None' with your implementation of the function

// Operators are represented by a discriminated union
type operator = Left | Right | Up | Down
with 
  // method 'Apply' attemps to apply the operator to input state 's'
  // it returns the next state, wrapped in 'Some' if the operator
  // is applicable to 's' and returns 'None' otherwise 
  member o.Apply s = 
    // 'right' implements the application of operator 'Right'
    let right { emptyPos = (r, c); board = b } = 
      if c = 3 then None
      else Some { emptyPos = (r, c + 1); board = b.Swap (r, c) (r, c + 1)}
    in
    // 'left' implements the application of operator 'Left'
    let left { emptyPos = (r, c); board = b } =
      if c = 1 then None
      else Some { emptyPos = (r, c - 1); board = b.Swap (r, c) (r, c - 1)}
    in
    // 'up' implements the application of operator 'Up'
    let up { emptyPos = (r, c); board = b } =
      if r = 1 then None
      else Some { emptyPos = (r - 1, c); board = b.Swap (r, c) (r - 1, c)}
    in
    // 'down' implements the application of operator 'Down'
    let down { emptyPos = (r, c); board = b } =
      if r = 3 then None
      else Some { emptyPos = (r + 1, c); board = b.Swap (r + 1, c) (r, c)}
    in
    match o with 
    | Left  -> left s
    | Right -> right s
    | Up    -> up s
    | Down  -> down s

// a successor of the sample goal state 
let b = 
  match Left.Apply goalState with
  | Some x -> x
  | _ -> goalState

goalState.ToString
b.ToString


(** Solution Plan **)

// A solution plan is encoded as an immutable record with a field containing
// a list of the actions that lead from the initial state to a goal state
type Plan = { actions: operator list }
with 
  // method 'Execute' executes the plan in 'p' starting from state 's'
  member p.Execute s = 
    let rec exec s l =
      match l with 
      | [] -> s
      | (op : operator) :: l' -> 
        match op.Apply s with
        | None -> failwith "Plan is infeasible"
        | Some s' -> exec s' l'
    in 
      exec s p.actions


// sample plan
let p = {actions = [Left; Up; Right; Up; Left; Down]}

// sample execution
let s2 = p.Execute s1

s1.Print
s2.Print


//-------------
// Problem A.2
//-------------
//
// Write a plan that goes from a state with the first configuration below
// to a state with the second. Verify that your plan is correct by running it
(*   
 +-------+
 | 2 6 1 |
 | 4   3 |
 | 7 5 8 |
 +-------+

 +-------+
 | 1 2 3 |
 | 4 5 6 |
 | 7 8   |
 +-------+

 *)

let myPlan = { actions = [Up; Right; Down; Left; Down; Right] }


(** Heuristics **)

//-------------
// Problem A.3
//-------------
//
// 'misplacedTiles' takes a goal state 'gs' and a current state 'cs' and
// returns the number of misplaced tiles in 'cs' with respect to 'gs'
let rec checkRow gs cs n m result =
  if m > gs.board.size then result
  else
    if gs.board.Value (n, m) = cs.board.Value (n, m) then checkRow gs cs n (m + 1) result
    else checkRow gs cs n (m + 1) (result + 1)

let rec checkCol gs cs n result =
  if n > gs.board.size then result
  else checkCol gs cs (n + 1) (checkRow gs cs n 1 result)

  // 1 start col
  // 0 start result
let misplacedTiles gs cs = 
    checkCol gs cs 1 0