(*==================================================
    CS:4420 Artificial Intelligence
    Spring 2018
    
    Homework 2 -- Part B
    
    Name: Thomas Han
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


let mutable Rand = System.Random 1

// Rerun the following assignment to reset the pseudo-random numer generator 
// to its initial state.
// Used a seed different from 1 if you want to get a different sequence of 
// pseudo-random numbers
Rand <- System.Random 1

//------------------------------
// Part B -- Genetic Algorithms
//------------------------------

// A position on the chess board is encoded just as a pair of integers.
// Each coordinate ranges from 1 to some n > 1.
// Position (1,1) is the top-leftmost position in the board.
type pos = int * int

// A chess board is encoded as an immutable record that stores in 
// field 'squares' exactly the positions that have a rook on them,
// File 'size' stores the number of rows/colums of the board.
// The 'chessBoard' type is equipped with several "member functions" (ie, methdods)

type chessBoard = 
  { size : int;
    squares : pos list
  }
with 
  member b.IsOccupied p = List.contains p b.squares
  member b.Add p =
    let _ = require (not (b.IsOccupied p)) "chessBoard.add" "square is occupied already" 
    in
    { b with squares = p :: b.squares } 
  member b.Remove p = 
    let _ = require (b.IsOccupied p) "chessBoard.remove" "square is not occupied" 
    in
    { b with squares = List.filter (fun x -> x <> p) b.squares }
  member b.SplitSquares c = 
    let _ = require (1 <= c && c < b.size) "chessBoard.split" "argument is out of range" 
    in
    List.partition (fun (_, j) -> 1 <= j && j <= c) b.squares
  member b.Attacks =
    let isUnderAttack (i,j) = 
      List.exists (fun (r,c) -> (r = i && c <> j) || (r <> i && c = j)) b.squares
    in
      List.fold (fun n p -> if isUnderAttack p then n + 1 else n) 0 b.squares
  member b.Pieces = b.squares.Length
  member b.ToString =
    let rec rowToString i j s = 
      match j with
      | 0 -> s
      | _ -> 
        let sq = (if List.contains (i,j) b.squares then "o" else " ") + "|" in
        rowToString i (j - 1) (sq + s)
    in
    let rec squaresToString i s =
      match i with
      | 0 -> s
      | _ -> 
        let row = " |" + (rowToString i b.size "\n") in
        squaresToString (i - 1) (row + s)
    in
      squaresToString b.size "\n\n"
  member b.Print = printf "%s" b.ToString
 

// Generates a new board of size s with up to s randomly positioned rooks on it
let randomBoard s = 
  let _ = require (s > 1) "randomBoard" "board size must be at least 2" 
  in
  let s' = s + 1 in
  let max = Rand.Next (1, s') in
  let rec loop n l =
    match n with 
    | 0 -> l
    | _ ->
      let p = (Rand.Next (1, s'), Rand.Next (1, s')) in
      let l' = if List.contains p l then l else p :: l in
      loop (n - 1) l'
  in
    { size = s; squares = loop max [] }

// sample random boards
let b1 = randomBoard 20
let b2 = randomBoard 20
let b3 = randomBoard 20

b1.Print
b2.Print
b3.Print

// Generates a random population of 'n' boards of size 's'
let randomPop s n =
  let f = "randomPop" in
  let _ = require (s > 1) f "the board size must be at least 2" in
  let _ = require (n > 1) f "the initial population size must be at least 2" 
  in  
  let rec initPop i l =
    match i with
    | 0 -> l
    | _ -> initPop (i - 1) ((randomBoard s) :: l) 
  in
  initPop n []

// sample random popolation
let pop1 = randomPop 4 11

// prints to the standard ouput every individual in input population 'pop'
let printPop p =
  List.iter (fun (x : chessBoard) -> x.Print) p

printPop pop1

//-------------
// Problem B.1
//-------------
//
// Function 'fitness' takes a chess board and returns a fitness value for it
// from the real interval [0 .. 1].
// The returned value should be inversely proportional to the number of rooks 
// under attack on the board. Also, it should be highest for n x n boards 
// with n rooks none of which are under attack.
let fitness (b : chessBoard): float = 
  float (b.Pieces - b.Attacks) / float (b.Pieces)
  

// sample board
let b = {size = 4; squares = [(1,1); (2, 2); (3, 3); (4, 4)]}

b.Print
fitness b
b.Pieces
b2.Attacks
b.size
b.squares


// 'select' takes a fitness function and a board population and 
// returns a selection of randomly chosen individuals from the population,
// with each individual chosen with a probability directly proportional
// to its fitness.
// Other selection strategies might be better.
let select fit pop = 
  let keep b = (Rand.NextDouble ()) <= (fit b) in
  List.filter keep pop    

// 'pair' returns a list of pairs of individuals from the input population 'pop'
// where each individual is randomly paired with another one.
// Note that it is possible for different individuals to be paired with
// the same one or even themselves.
// Other pairing strategies might be better. 
let pair (pop : 'a list) =
  let max = pop.Length in
  let pair x =
    let n = Rand.Next max in
    let y = List.item n pop in
    (x, y)
  in
  List.map pair pop


// sample pairing
let pairs = pair pop1

// 'crossover' takes a list of pairs of individuals and returns a list
// of individuals generated by crossover. The crossover point is chosen
// randomly for each pair.
let crossover max l = 
  let rec cross (pairs : (chessBoard * chessBoard) list) l = 
    match pairs with
    | [] -> l
    | (b1, b2) :: pairs' -> 
      let n = Rand.Next (1, max) in
      let (s11, s12) = b1.SplitSquares n in
      let (s21, s22) = b2.SplitSquares n in
      let b3 = { size = max; squares = s11 @ s22} in 
      let b4 = { size = max; squares = s21 @ s12} in 
      cross pairs' (b3 :: b4 :: l) 
  in
  cross l []

// sample crossover list
let pop2 = crossover 4 pairs

printPop pop2

// 'isSol' takes a board and returns true iff the board is a solution board
// (it contains the maximum number of rooks with none of them under attack)
let isSol (b : chessBoard) = 
  b.Pieces = b.size && b.Attacks = 0


isSol b

b.Pieces
b.size
b.Attacks

b.Print

// 'findSol' takes a board population and returns, wrapped in 'Some', an individual
// that is a solution if any; otherwise, it returns None. 
let rec findSol pop =
  match pop with
  | [] -> None
  | b :: pop' -> if isSol b then Some b else findSol pop'  


findSol [b1; b]


// 'mutate' takes a board 'b' and returns a mutated version of 'b'
// obtained by removing or adding a rook
let mutate (b : chessBoard) = 
  let newIndex () = Rand.Next (1, b.size + 1) in
  let p = (newIndex (), newIndex ()) in
  if b.IsOccupied p then 
    b.Remove p 
  else 
    b.Add p

b1.Print

let b1' = mutate b1
b1'.Print

// 'reproduce' takes a board population 'pop' and returns a new one  
// obtained from 'pop' by crossover and mutation
let reproduce bsize pop =  
  let pairs = pair pop in
  let pop' = crossover bsize pairs in
  List.map mutate pop'

printPop pop1
let pop3 = reproduce 4 pop1

printPop pop3

// 'generic' implements a version of the basic genetic algorithm from the book/notes.
// 
let genetic limit pop (fit: chessBoard -> float) =
  let _ = require (limit > 0) "genetic" "iteration limit must be positive" in
  let _ = require (pop <> []) "genetic" "Initial population must be non-empty" 
  in  
  let bsize = pop.Head.size in 
  let rec repeat n pop =
    // let _ = (printf "----------------------\n"; printPop pop)
    // in
    match (findSol pop, n) with
    | Some i, _ -> Some i
    | None, 0 -> None
    | _ ->
      let parents = select fit pop in
      let pop' = reproduce bsize parents in 
      repeat (n - 1) pop'
  in
    repeat limit pop

let pop = randomPop 5 20
let s = genetic 30 pop fitness 

s.Value.Print


//-------------
// Problem B.2
//-------------
(*
// (a)

// only a (one) solution is listed for each diferent sizes.

2 x 2

 |o| |
 | |o|

3 x 3

 | |o| |
 |o| | |
 | | |o|

4 x 4

 | |o| | |
 |o| | | |
 | | |o| |
 | | | |o|

// (b)  
  How many random samples I generate definitely affects how fast I can get a solution.
  Also, if the iteration limit is too low, getting a solution becomse significant slower.

// (c)
  
 | | |o| | |
 | | | | |o|
 | | | |o| |
 |o| | | | |
 | |o| | | |

  I have to increase the value of limit and increase the number of random boards generated.

*)