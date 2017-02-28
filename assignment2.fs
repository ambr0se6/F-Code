module Hw2

(* Assignment 2 *) (* Do not edit this line. *)
(* Student name: Ambrose Ryan Burgett, Id Number: 260615904 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  It is OK to change a "rec" declaration and put the
recursive function inside a helper if you want to.  Your code MUST compile
and must NOT go into infinite loops.  An assignment like that means you
have not tested it.  You will get ZERO FOR THE ENTIRE ASSIGMENT even if the
problem is only with one question.  If you are not able to get the code to
compile and run do not submit it.  *)

(* Question 1 *) 

let deriv (f, dx: float) = fun x -> ((f(x + dx) - f(x))/dx)

//f:(float -> float) * guess:float * tol:float * dx:float -> float

let rec newton(f,guess:float,tol:float,dx:float) =  
  let fprime = deriv(f, dx)
  let xnplus1 = (guess - f(guess)/(fprime guess)) 
  if abs (f xnplus1) < tol then xnplus1 
  else newton(f,xnplus1,tol,dx)


// let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c)

// let result = newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001)

// let root = newton(sin,5.0,0.0001,0.0001)


(* Question 2 *)

type term = Term of float * int
type poly = Poly of (float * int) list

exception EmptyList

let Simplify(Poly p:poly):poly =
  if p.IsEmpty then raise EmptyList 
  let rec helper(Poly p:poly):(float * int) list = 
    let newP = List.sortBy (fun (_, y) -> -y) p
    match newP with
    | [] -> []
    | (a,b)::(c,d)::z ->
      if (b=d && (a+c=0.0)) then helper(Poly z)
      elif b=d then (a+c,b)::helper(Poly z)
      else (a,b)::helper(Poly ((c,d)::z))
    | (x,y)::z -> (x,y)::helper(Poly z)
  Poly (helper(Poly p:poly))


let multiplyPolyByTerm(Term (c,e):term, Poly p:poly):poly =
  if p.IsEmpty then raise EmptyList
  let rec helper(Term (c,e):term, Poly p:poly) =
    match p with
    | [] -> []
    | (x,y)::z -> (c*x,y+e)::helper(Term (c,e), Poly z)
  Poly (helper(Term (c,e), Poly p))

//This needs to handle negative terms being added 
let addTermToPoly(Term (c,e):term, Poly p:poly):poly = 
  if p.IsEmpty then raise EmptyList
  let rec helper(Term (c,e):term, Poly p:poly) = 
    match p with 
    | [] when c>0.0-> [(c,e)]
    | [] when c<=0.0 -> []
    | (x,y)::z -> 
      if (y=e && (x+c=0.0)) then helper(Term (c,e), Poly z)
      elif y=e then (c+x,y)::helper(Term (c,e), Poly z)
      else (x,y)::helper(Term(c,e), Poly z) 
  Simplify(Poly (helper(Term (c,e), Poly p)))

let addPolys(Poly p1:poly, Poly p2:poly):poly =
  if (p1.IsEmpty || p2.IsEmpty) then raise EmptyList
  let rec helper(Poly p1:poly, Poly p2:poly) = 
    match p1,p2 with 
    | ([],[]) -> []
    | (_,[]) -> []
    | [],_ -> []
    | ((x,y)::z),p2 -> 
      let (Poly newP2) = addTermToPoly(Term (x,y), Poly p2)
      if (p2 = newP2) then (x,y)::helper(Poly z, Poly p2) 
      else newP2@helper(Poly z, Poly p2) 
  Simplify (Poly (helper(Poly p1, Poly p2)))

let multPolys(Poly p1:poly, Poly p2:poly) = 
  if (p1.IsEmpty || p2.IsEmpty) then raise EmptyList
  let rec helper(Poly p1:poly, Poly p2:poly):((float * int) list) = 
    match p1,p2 with 
    | ([],[]) -> []
    | (_,[]) -> []
    | [],_ -> []
    | ((x,y)::z),p2 -> 
      let (Poly multP) = multiplyPolyByTerm(Term (x,y), Poly p2)
      multP@helper(Poly z, Poly p2)
  Simplify (Poly (helper(Poly p1, Poly p2)))

let exp(b:float, e:int) =
  let rec helper(b:float, e:int, a: float) =
    if (b = 0.0) then 0.0
    elif (e = 0) then a
    elif (e % 2 = 1) then helper(b,e-1, b*a)
    else helper(b*b,e/2,a)
  helper(b,e,1.0)

let evalTerm (v:float) (Term (c,e):term) = if (e=0) then c else c * exp(v,e)

let evalPoly(Poly p:poly,v:float):float = 
  if p.IsEmpty then raise EmptyList
  let rec helper(Poly p:poly, v:float):float = 
    match p with 
    | [] -> 0.0
    | (x,y)::z -> ((evalTerm (v) (Term (x,y))) + helper(Poly z, v))
  helper(Poly p, v)

let diffPoly (Poly p) = 
  if p.IsEmpty then raise EmptyList
  let rec helper(Poly p) = 
    match p with
    | [] -> []
    | (x,y)::z ->
      if y=0 then helper(Poly z)
      else (x*(float y),y-1)::helper(Poly z)
  Poly (helper(Poly p)) 

(* Question 3 *)
type Exptree =
  | Const of int 
  | Var of string 
  | Add of Exptree * Exptree 
  | Mul of Exptree * Exptree

type Bindings = (string * int) list

(* exception notFound *)

//name:string * env:Bindings -> int option
let rec lookup(name:string, env: Bindings) = 
  match env with
  | [] -> None
  | (strng, intgr)::lst -> 
    if (strng.Equals(name)) then Some intgr
    else lookup(name,lst)

//name:string * value:int * b:Bindings -> (string * int) list
let rec insert(name:string, value: int, b: Bindings) = 
  match b with 
  | [] -> [(name, value)]
  | (strng, intgr)::lst -> 
    if(name < strng) then (name, value)::lst 
    else (strng,intgr)::insert(name, value, lst)

let (|SomePair|NonePair|) (a, b) =
  match (a, b) with
  | None, _
  | _, None -> NonePair
  | Some a, Some b -> SomePair (a, b)

let searchBinding (name:string, env: Bindings)= lookup(name, env)

// exp:Exptree * env:Bindings -> int option                                           
let rec eval(exp : Exptree, env:Bindings) = 
    match exp with
    |Const x -> Some x
    |Var x ->  searchBinding(x,env) 
    | Add (x, y) -> 
      match eval(x, env), eval(y, env) with
      | NonePair -> None
      | SomePair (x, y) -> Some (x + y)
    | Mul (x, y) -> 
      match eval(x, env), eval(y, env) with
      | NonePair -> None
      | SomePair (x,y) -> Some (x * y) 
 
(* for testing
 
let env:Bindings = [("a",3);("b",4);("c",5)]                                

let exp1 = Add(Const 3, Const 4)
let exp2 = Add(Const 3, Var "b")
let exp3 = Add(Var "c", Var "b")
let exp4 = Mul(exp3,exp2)
let exp5 = Add(Var "d",exp3)
let env2 = insert("b",10,env)

*)


(* Question 4 *)

type Team = string
type Goals = Goals of int
type Points = Points of int
type Fixture = Team * Team  
type Result = ((Team * Goals) * (Team * Goals))
type Table = Map<Team,Points>
    
let league =
  ["Chelsea"; "Spurs"; "Liverpool"; "ManCity"; "ManUnited"; "Arsenal"; "Everton"; "Leicester"]
  
//(Team * Goals) * (Team * Goals) -> (Team * Points) * (Team * Points)
let pointsMade (r: Result) =
  match r with 
  | ((t1,Goals g1),(t2, Goals g2)) -> 
         if (g1 > g2) then ((t1, Points 3),(t2, Points 0))
         elif (g2 > g1) then ((t1, Points 0),(t2, Points 3))
         else ((t1, Points 1),(t2, Points 1))

let updatePoints(Points p1:Points,Points p2:Points):int = p1+p2
  
let initEntry (name:Team) = (name, Points 0)
           
let initializeTable l = Map.ofList (List.map initEntry l)

let weekend1:Result list = [(("Chelsea", Goals 2),("Spurs", Goals 1)); (("Liverpool", Goals 3),("ManCity", Goals 2));(("ManUnited", Goals 1),("Arsenal", Goals 4));(("Everton", Goals 1),("Leicester", Goals 5))]

let weekend2:Result list = [(("Chelsea", Goals 5),("Arsenal", Goals 0)); (("Spurs", Goals 3),("ManCity",Goals 2)); (("ManUnited", Goals 1),("Liverpool", Goals 0));(("Everton", Goals 3),("Leicester", Goals 5))]

let s = [weekend2;weekend1]

let updateTable(t:Table,r:Result):Table = 
  let newR = pointsMade(r)
  match newR with 
  | ((t1,Points g1),(t2, Points g2)) -> 
    let curV1 = t.TryFind t1
    let curV2 = t.TryFind t2
    let temp = t.Add(t1,Points (updatePoints(Points g1, Option.get curV1)))
    let newTemp = temp.Add(t2,Points (updatePoints(Points g2, Option.get curV2)))
    newTemp

let rec weekendUpdate(t:Table,rl: Result list): Table = 
  match rl with
  | [] -> t
  | result::restResults ->  weekendUpdate(updateTable(t,result),restResults)


let rec seasonUpdate(t:Table, sll:Result list list) : Table = 
  match sll with 
  | [] -> t 
  | reslst::restOfResLst -> seasonUpdate(weekendUpdate(t, reslst),restOfResLst)


let less((s1,n1):Team * Points, (s2,n2):Team * Points) =  
  match n1,n2 with
  | (Points x, Points y) -> 
    if (x>y) then false
    else true

let rec myinsert item lst =
  match lst with
  | [] -> [item]
  | x::xs -> if less(item,x) then x::(myinsert item xs) else item::lst

let rec isort lst =
  match lst with
  | [] -> []
  | x::xs -> myinsert x (isort xs)

let showStandings (t:Table) = isort (Map.toList t)
                                                  
(* Question 5 *)

type Destination = City of string
type RoadMap = Roads of Map<Destination, Set<Destination>>

let roadData = [
  "Andulo", ["Bibala"; "Cacolo"; "Dondo"]
  "Bibala", ["Andulo"; "Dondo"; "Galo"]
  "Cacolo", ["Andulo"; "Dondo"]
  "Dondo",  ["Andulo"; "Bibala"; "Cacolo"; "Ekunha"; "Funda"]
  "Ekunha", ["Dondo"; "Funda"]
  "Funda",  ["Dondo"; "Ekunha"; "Galo"; "Kuito"]
  "Galo",   ["Bibala"; "Funda"; "Huambo"; "Jamba"]
  "Huambo", ["Galo"]
  "Jamba",  ["Galo"]
  "Kuito",  ["Ekunha"; "Funda"]
]
// data:(string * string list) list -> RoadMap
let makeRoadMap data = 
  data
  |> List.map (fun (strng, strnglst) -> (City strng, strnglst |> List.map City |> Set.ofList))
  |> Map.ofList
  |> Roads

//RoadMap -> n:int -> startCity:Destination -> Set<Destination>
let rec upToManySteps (Roads r) n startCity =
  let neighbrs =
    match r.TryFind startCity with 
    | None -> failwith "The startCity is not a valid city in Prakash Land"
    | Some value -> value
  match n with 
  | n when n>1 -> 
    let answer = Set.fold( fun acc place -> Set.union (upToManySteps (Roads r) (n-1) place) acc) Set.empty neighbrs
    answer
  | n when n=1 -> neighbrs
  | n when n=0 -> 
    let baseCase = Set.empty:Set<Destination>
    baseCase.Add startCity
  | n when n<0 -> failwith "Too deep into the Recursive Call"
  

    





