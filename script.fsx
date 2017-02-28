printf "Testing..."

#load "assignment2.fs"
open Hw2

let make_cubic(a:float,b,c) = fun x -> (x*x*x + a * x*x + b*x + c)

let result = newton(make_cubic(2.0,-3.0,1.0),0.0,0.0001,0.0001)

let root = newton(sin,5.0,0.0001,0.0001)
// let t1 = Term (3.0,6)

// let p1 = Poly [(3.0,5);(2.0,2);(7.0,1);(1.5,0)]

// let p3 = multiplyPolyByTerm(t1, p1)

// let result3 = addTermToPoly(t1,p1)

// let result4 = addPolys(p1,p3)

// let result5 = multPolys(p1, p3)

// let result6 = evalPoly(p1,1.0)

// let result7 = diffPoly(p1) 

// let env:Bindings = [("aa",1);("ab",2);("ab",3);("ac",4);("ac",5);("ac",6)]  

// let result9 = lookup("aac",env)

// let result8 = Sort(env)

// let result10 = insert("ab", 4, env)

// let result11 = lookup("ab",result10)

//let result11 = eval(Add(Var "y", Const 3), [("y",2)])

// let result12 = initializeTable league

// let scores = (("Chelsea", Goals 5),("Arsenal", Goals 0)):Result 

// let result13 = updateTable(result12,scores)

// let result14 = weekendUpdate(result12, weekend1)

// let result15 = seasonUpdate(result12, s)

// let result16 = showStandings(result15)

//let flatten x = set.fold set.union set.empty
 
// let result17 = makeRoadMap roadData

// let result = upToManySteps (makeRoadMap roadData) 0 (City"Huambo":Destination)

// let result1 = upToManySteps (makeRoadMap roadData) 2 (City"Kuito":Destination)


printf "Done!"