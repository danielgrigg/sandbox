let isIthChar (str:string) i ch = str.[i] = ch

printfn "%A" (isIthChar "foo" 1 'o')
printfn "%A" (isIthChar "foo" 1 'x')

let rec occFromIth (str:string) i ch =
  if (i >= str.Length) then 0
  elif (isIthChar str i ch) then 1 + (occFromIth str (i+1) ch)
  else (occFromIth str (i+1) ch)

printfn ": %A" (occFromIth "abc" 5 'c')
printfn ": %A" (occFromIth "abc" 2 'c')
printfn ": %A" (occFromIth "abc" 1 'c')
printfn ": %A" (occFromIth "abcdefabcabc" 1 'c')
printfn ": %A" (occFromIth "abcdefabcabc" 3 'c')

let occInString str c = occFromIth str 0 c

printfn "OccInString abcaabcccd a: %A" (occInString "abcaabcccd" 'a')
printfn "OccInString abcaabcccd b: %A" (occInString "abcaabcccd" 'b')
printfn "OccInString abcaabcccd c: %A" (occInString "abcaabcccd" 'c')
printfn "OccInString abcaabcccd d: %A" (occInString "abcaabcccd" 'd')


let notDivisible d n = n % d <> 0

printfn "notDivisible 2 5 %A" (notDivisible 2 5)
printfn "notDivisible 3 9 %A" (notDivisible 3 9)

let rec test a b c = 
  match (a,b,c) with 
  | (a,b,c) when (a <= b) -> (notDivisible a c) && (test (a+1) b c)
  | _ -> true 

let rec test2 a b c =
  if (a <= b) then (notDivisible a c) && (test2 (a+1) b c)
  else true

printfn "test 2 2 2: %A" (test 2 2 2)
printfn "test 2 2 5: %A" (test 2 2 5)
printfn "test 3 5 13: %A" (test 3 5 13)

printfn "test2 3 3 8: %A" (test2 3 3 8)
printfn "test2 3 4 8: %A" (test2 3 4 8)

let rec prime = function
  | 1 | 2 -> true
  | n when (n > 1) -> test 2 (n-1) n
  | _ -> false

let rec nextPrime = function
  | n when (prime (n+1)) -> n+1
  | n -> nextPrime (n+1)

let rec bin = function
  | (n, k) when (n = k || k = 0) -> 1
  | (n, k) when (n <> 0 && k <> 0 && n > k) -> bin (n-1,k-1) + bin (n-1, k)
  | _ -> 0 

let rec f = function
  | (0,y) -> y
  | (x,y) -> f(x-1, x*y);;


let VAT n x = x * (1.0 + 0.01 * (float n))
let unVAT n x = x / (1.0 + 0.01 * (float n))

let min f = 
  let rec min2 f n = if (f n = 0) then n else min2 f (n+1)
  min2 f 1

let curry f x y = f (x,y) 
let uncurry g (x,y)  = g x y

let (<<.) (h1,m1,f1) (h2,m2,f2) = 
  match (f1,f2) with
    | ("AM","AM") -> (h1,m1) < (h2,m2)
    | ("PM","PM") -> (h1,m1) < (h2,m2)
    | ("AM","PM") -> true
    | ("PM","AM") -> false
    | _ -> failwith "error"


type Time = { f:string; h:int; m:int }

let (<.) x y = 
  match (x.f,y.f) with
    | ("AM","AM") -> x < y
    | ("PM","PM") -> x < y
    | ("AM","PM") -> true
    | ("PM","AM") -> false
    | _ -> failwith "error"

type Time2 = 
  | AM of hour:int * minute:int
  | PM of hour:int * minute:int

let timeLessThan t1 t2 =
  match (t1, t2) with 
  | (AM (h1,m1), AM (h2,m2)) -> (h1,m1) < (h2,m2)
  | (PM (h1,m1), PM (h2,m2)) -> (h1,m1) < (h2,m2)
  | (AM _, PM _) -> true
  | (PM _, AM _) -> false


type Currency = { pounds:int; shillings:int; pence:int}
type Currency2 = | Currency2 of pounds:int * shillings:int
type Currency3 = int*int*int


let addCurrency c1 c2 =
  let pence (a,b,c) = a*20*12 + b*12 + c
  let shilling p = p / 12
  let pound p = (shilling p ) / 20
  let penceSum = pence c1 + pence c2 
  (pound penceSum, shilling penceSum % 20, penceSum % 12)

let addCurrency2 c1 c2 =
  let pence x = x.pounds*20*12 + x.shillings*12 + x.pence
  let shillings x = x / 12
  let pounds x = (shillings x) / 20
  let penceSum = pence c1 + pence c2
  {pounds = pounds penceSum; 
   shillings = shillings penceSum % 20;
   pence = penceSum % 12}

let (.+.) (a:float,b:float) (c:float,d:float) = (a + c, b + d)
let (.*.) (a:float,b:float) (c:float,d:float) = (a*c - b*d, b*c + a*d)
let (-.) (a:float,b:float) = (-a,-b)
let (.-.) x y = x .+. (-.) y
let (./.) x (a:float,b:float) = 
  let l = a*a + b*b
  x .*. (a/l, -b/l) 


type StraightLine = { a:float; b:float }
let mirrorY l = { a = -l.a; b = l.b }
let str l = sprintf "y = %fx + %f" l.a l.b

type Solution = 
  | TwoRoot of float*float
  | OneRoot of float
  | NoRoot

type Equation = float*float*float

let solve (a,b,c) =
  let d = b*b - 4.0*a*c
  if d < 0.0 || a = 0.0 then NoRoot
  else 
    let sqrtD = sqrt d
    if sqrtD =  0.0 then OneRoot (-b / (2.0*a))
    else TwoRoot ((-b + sqrtD)/(2.0*a), (-b - sqrtD)/(2.0*a))
 

type Shape = 
  | Circle of radius:float
  | Square of width:float
  | Triangle of a:float * b:float * c:float

let area = function
  | Circle r when r > 0.0 -> System.Math.PI * r * r
  | Square w when w > 0.0 -> w * w
  | Triangle (a,b,c) 
    when a > 0.0 && b > 0.0 && c > 0.0 && 
         a < b + c && b < c + a && c < a + b -> 
    let s = (a + b + c) / 2.0
    sqrt(s * (s-a) * (s-b)*(s-c));;

