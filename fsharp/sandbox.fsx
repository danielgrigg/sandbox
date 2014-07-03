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
  | (a,b,c) when (a <= b) -> true
  | _ -> false 
