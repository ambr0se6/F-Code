(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Ambrose Ryan Burgett, Id Number: 260615904 *) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like, except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code MUST compile and must NOT go into infinite
loops.  An assignment like that means you have not tested it.  You will get
ZERO FOR THE ENTIRE ASSIGMENT even if the problem is only with one
question.  If you are not able to get the code to compile and run do not
submit it.  *)

(* module hw1_sol.  Use this if you want to load the file into an interactive session.*)

(* Question 1 *) (* Do not edit this line. *)

// l:float list -> float
let rec sumlist l = 
  match l with
  | [] -> 0.0
  | x::xs -> x+sumlist xs

//’a list * ’b list -> (’a * ’b) list
let rec pairlists twolists =
  match twolists with
    | ([],[]) -> []
    | ([], _)
    | (_, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> (x,y)::pairlists(xs, ys) 
                        

// weights:float list -> data:float list -> float                        
let w_mean weights data = 
  let rec weightMe weightsAndData = 
    match weightsAndData with 
    | ([],[]) -> 0.0
    | ([],_) 
    | (_, []) -> failwith "Error -- lists are not of the same length"
    | (x::xs, y::ys) -> x*y + weightMe(xs, ys)
  (weightMe(weights, data)) / (sumlist weights)

  
(* Question 2. *)  (* Do not edit this line. *)


// ’a * ’a list -> bool when ’a : equality
let rec memberof pair = 
  match pair with
  | (x,[]) -> false 
  | (x, y::ys) -> 
    if x=y then true 
    else memberof(x, ys)

//’a * ’a list -> ’a list when ’a : equality
let rec remove(item, lst) = 
  match lst with
  | [] -> []
  | x::xs -> 
    if x<>item then (x)::remove(item, xs) 
    else remove(item, xs)


(* Question 3. *)  (* Do not edit this line. *)

//l:’a list -> ’a when ’a : comparison
let findMax l = 
  let rec helper(l,m) = 
    match l with 
    | [] -> m
    | x::xs -> 
      if x > m then helper(xs, x) 
      else helper(xs, m)
  match l with
  | [] -> failwith "Error -- empty list"
  | (x::xs) -> helper(xs,x)


(* Question 4. *)  (* Do not edit this line. *)
  
//l:’a list -> ’a list when ’a : comparison
let rec selsort l =
  match l with 
  | [] -> []
  | x::xs ->  
    let max = findMax l
    (max)::selsort (remove(max, l))

(* Question 5. *)  (* Do not edit this line. *)

//’a list * ’a list -> ’a list when ’a : equality
let rec common twolists = 
  match twolists with 
  | ([],[]) -> []
  | ([], _) -> []
  | (_, []) -> []
  | (x::xs, y::ys) -> 
    if x=y then (x)::common(xs, ys) 
    elif memberof (x, ys) then (x)::common(xs, ys)
    else common(xs, ys)
                  
(* Question 6. *)   (* Do not edit this line. *)

(* Mergesort requires that you use recursion.  Using isort or
some other sort defeats the whole purpose.*)

//l:’a list -> ’a list * ’a list
let rec split l = 
  match l with
  | [] -> ([],[])
  | [x] -> ([x],[])
  | x1::x2::xs -> 
    let l1, l2 = split xs
    x1::l1, x2::l2

//’a list * ’a list -> ’a list when ’a : comparison
let rec merge twolists = 
  match twolists with 
  | ([],[]) -> []
  | (l1,[]) -> 
    let x = findMax l1
    let xs = remove (x,l1) 
    merge(xs,[])@[findMax l1]
  | ([],l2) -> 
    let x = findMax l2
    let xs = remove (x,l2) 
    merge(xs,[])@[findMax l2]
  | (l1, l2) -> 
    let x = findMax l1
    let xs = remove (x,l1) 
    let y = findMax l2
    let ys = remove (y,l2)
    if x>=y then merge(xs,l2)@[x] 
    else merge(l1,ys)@[y]

//l:’a list -> ’a list when ’a : comparison
let rec mergesort l = 
  match l with
  | [] -> []
  | ([n]) -> [n] (* Without this you will go into an infinite loop. *)
  | n::ns -> 
    let (l1,l2) = split l
    merge (mergesort l1, mergesort l2)




