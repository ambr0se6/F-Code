//val repeat : f:(’a -> ’a) -> n:int -> (’a -> ’a)
let repeat f n = 
  match n with 
  | 0 -> id
  | _ -> fun x -> ((repeat f (n-1)) x)