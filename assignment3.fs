module Hw3

(* Assignment 3 *) (* Do not edit this line. *)
(* Student name: Ambrose Ryan Burgett, Id Number: 260615904 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  It is OK to change a "rec" declaration and put the
recursive function inside a helper if you want to.  Your code MUST compile
and must NOT go into infinite loops. *)

(* Question 1 *)
type Cell = { data : int; next : RList}
and RList = Cell option ref

let c1 = {data = 1; next = ref None}
let c2 = {data = 2; next = ref (Some c1)}
let c3 = {data = 3; next = ref (Some c2)}
let c5 = {data = 5; next = ref (Some c3)}

(* This converts an RList to an ordinary list, which is then displayed. *)
let rec displayList (c : RList) =
  match !c with
    | None -> []
    | Some { data = d; next = l } -> d :: (displayList l)

(* This converts a cell to a list.  You may find it useful for testing.  No need to
use it in your solution. *)

let cellToRList (c:Cell):RList = ref (Some c)

(* This is what you need to code. *)
//val reverse : lst:RList -> RList
let reverse (lst: RList) =
  let crntRList = ref lst
  let rec helper(crntRList:RList, prevRList:RList) = 
    match !crntRList with
    | None -> prevRList
    | Some{data = d; next = nextRList} -> 
      crntRList := Some{data = d; next = prevRList}
      helper(nextRList, crntRList)
  helper(lst,(ref None))
    
let result = cellToRList c5 |> reverse |> displayList


(* Question 2*)

type transaction = Withdraw of int | Deposit of int | CheckBalance | ChangePassword of string | Close
//openingBalance:int * password:string -> (string * transaction -> unit)
let makeProtectedAccount(openingBalance: int, password: string) = 
  let balance = ref openingBalance
  let pswd = ref password
  let mutable dead = false;
  let func = ref (fun (password: string, t: transaction) -> ())
  let closed = fun (password: string, t: transaction) ->
          match password with
          | _ when password <> !pswd -> printfn "Incorrect password"
          | _ when password.Equals(!pswd) -> 
            match t with
            | Withdraw(m) ->  printfn "Account closed"
            | Deposit(m) -> printfn "Account closed"
            | CheckBalance -> printfn "Account closed"
            | ChangePassword(s) -> printfn "Account closed"
            | Close -> printfn "Account closed"
  let solution = fun (password: string, t: transaction) ->
    match password with
    | _ when password <> !pswd -> printfn "Incorrect password"
    | _ when password.Equals(!pswd) -> 
      match t with
      | Withdraw(m) ->  
        if (!balance > m)
        then
          balance := !balance - m
          printfn "Balance is %i" !balance
        else
          printfn "Insufficient funds."
      | Deposit(m) -> (balance := !balance + m; (printf "Balance is %i\n" !balance))
      | CheckBalance -> (printf "Balance is %i\n" !balance)
      | ChangePassword(s) -> pswd := s
      | Close -> 
        func := closed
        dead <- true
        printfn "Account successfully closed"
  if dead then func := solution
  else func := closed
  !func                  

(* Question 3 *)

open System.Collections.Generic;;

type ListTree<'a> = Node of 'a * (ListTree<'a> list)

let bfIter f ltr = 
  let que = new Queue<ListTree<'a>>()
  que.Enqueue(ltr)
  while que.Count <> 0 do 
    let (prnt,chld) = 
      match que.Dequeue() with
      | Node(a,b) -> (a,b)
    f prnt 
    for i in chld do 
      let (prnt,chld) = 
        match i with
        | Node(a,b) -> (a,b)
      f prnt 



(* Some examples I used for testing.  *)
let n5 = Node(5,[])
let n6 = Node(6,[])
let n7 = Node(7,[])
let n8 = Node(8,[])
let n9 = Node(9,[])
let n10 = Node(10,[])
let n12 = Node(12,[])
let n11 = Node(11,[n12])
let n2 = Node(2,[n5;n6;n7;n8])
let n3 = Node(3,[n9;n10])
let n4 = Node(4,[n11])
let n1 = Node(1,[n2;n3;n4])

(* Just for testing, not needed for your solution. *)
let showNode n =
  match n with
    | Node(i,_) -> (printfn "%i" i)

    
bfIter (fun n -> printfn "%i" n) n1

    






        


        

      