
type Fexpr = 
  | Const of float
  | X
  | Add of Fexpr * Fexpr
  | Sub of Fexpr * Fexpr
  | Mul of Fexpr * Fexpr
  | Div of Fexpr * Fexpr
  | Sin of Fexpr
  | Cos of Fexpr
  | Log of Fexpr
  | Exp of Fexpr

let rec D = function
  | Const _ -> Const 0.0
  | X -> Const 1.0
  | Add(fe,ge) -> Add(D fe, D ge)
  | Sub(fe,ge) -> Sub(D fe, D ge)
  | Mul(fe,ge) -> Add(Mul(D fe, ge), Mul(fe, D ge))
  | Div(fe,ge) -> Div(Sub(Mul(D fe, ge), Mul(fe, D ge)), Mul(ge,ge))
  | Sin fe -> Mul(Cos fe, D fe)
  | Cos fe -> Mul(Const -1.0, Mul(Sin fe, D fe))
  | Log fe -> Div(D fe, fe)
  | Exp fe -> Mul(Exp fe, D fe)


 

let reduce0 = D(Mul(Const 3.0, Exp X))
let expect0 = Mul(Const 3.0, Exp X)
let reduce1 = Mul(Const 1.0, Exp (Const 1.0))


let rec red = function
  | Mul(Const 0.0,_) -> Const 0.0
  | Mul(Const 1.0,ge) -> red ge
  | Mul(_, Const 0.0) -> Const 0.0
  | Mul(fe, Const 1.0) -> red fe
  | Mul(fe, ge) -> Mul(red fe, red ge)
  | Add(Const 0.0, ge) -> red ge
  | Add(e, Const 0.0) -> red e
  | Add(fe, ge) -> Add(red fe, red ge)
  | Exp fe -> Exp (red fe)
  | e -> e 


let rec toString = function
  | Const x -> string x
  | X -> "x"
  | Add(e1,e2) -> (arg e1) + " + " +  (arg e2)
  | Sub(e1,e2) -> (arg e1) + " - " + (arg2 e2)
  | Mul(e1,e2) -> (arg e1) + " * " + (arg2 e2)
  | Div(e1,e2) -> (arg e1) + " / " + (arg2 e2)
  | Sin e -> "sin(" + (toString e) + ")"
  | Cos e -> "cos(" + (toString e) + ")"
  | Log e -> "log(" + (toString e) + ")"
  | Exp e -> "exp(" + (toString e) + ")"
and arg e = 
  match e with
  | Const _ -> toString e
  | X -> toString e
  | _ -> brackets e    
and arg2 e = 
  match e with
  | Add _ -> brackets e
  | Sub _ -> brackets e
  | _ -> toString e
and brackets e = "(" + (toString e) + ")"
 

let rec postfixStr = function
  | Add(e1,e2) -> "+ " + "(" + (postfixStr e1) + ") "  + "(" + (postfixStr e2) + ")"
  | Sub(e1,e2) -> "- " + "(" + (postfixStr e1) + ") "  + "(" + (postfixStr e2) + ")"
  | Mul(e1,e2) -> "* " + "(" + (postfixStr e1) + ") "  + "(" + (postfixStr e2) + ")"
  | Div(e1,e2) -> "/ " + "(" + (postfixStr e1) + ") "  + "(" + (postfixStr e2) + ")"
  | e -> toString e

let e1 = Add(X, Const 7.0)
let e2 = Mul(Add(X, Const 7.0), Sub(X, Const 5.0))
let e3 = Sub(X, Const 2.0)
let e4 = Sub(X, Mul(Const 2.0, X))
let e5 = Div(Add(e4, e3), Sub(e2, e1))

type BinTree<'a,'b> = 
  | Leaf of 'a
  | Node of BinTree<'a, 'b> * 'b * BinTree<'a,'b>

let rec leafVals = function
  | Leaf x -> set [x]
  | Node (l, x, r) -> Set.union (leafVals l) (leafVals r)

let rec nodeVals = function
  | Leaf _ -> Set.empty
  | Node (l, x, r) -> Set.union (nodeVals l) (nodeVals r) |> Set.add x

let vals t = (leafVals t, nodeVals t)

let bt1 = Node(Node(Leaf 1, "cd", Leaf 2), "ab", Leaf 3)
let bt2 = Node(Node(Leaf 1, "cd", Leaf 2), "ab", Leaf 2)


type AncTree = | Unspec | Info of AncTree * string * AncTree

let grandpa1 = Info(Unspec, "grandpa1", Unspec)
let grandma1 = Info(Unspec, "grandma1", Unspec)
let grandma2 = Info(Unspec, "grandma2", Unspec)
let at1 = Info(Info(grandpa1, "dad", grandma1) ,"dan", Info(Unspec, "mum", grandma2))


let drawAncTree t = 
  let rec ancestors d = function
    | Unspec -> []
    | Info (l, x, r) -> (d, string x)::(ancestors (d+1) l) @ (ancestors (d+1) r)
  let drawNode (dMax,sOut) (d, s) =
    if d > dMax then (d, sOut + "\n" + s) else (dMax, sOut + " " + s)

  (ancestors 0 t) |> List.sort |> List.fold drawNode (0, "") |> snd

type Sex = | Male | Female

let rec sexAnc isMale sex = function
  | Unspec -> []
  | Info (l, x, r) -> match (isMale, sex) with
                      | (true, Male) -> x::(rest sex l r)
                      | (true, Female) -> rest sex l r
                      | (false, Male) -> rest sex l r
                      | (false, Female) -> x::(rest sex l r)
and rest sex' l' r' = (sexAnc true sex' l') @ (sexAnc false sex' r')

let maleAnc t = sexAnc false Male t
let femaleAnc t = sexAnc true Female t


type SearchTree<'a when 'a : comparison> = 
  | SLeaf 
  | SNode of SearchTree<'a> * 'a * SearchTree<'a>

let rec deleteSmallest = function
  | SLeaf -> failwith "tree invariant - must delete a node"
  | SNode(SLeaf,a,r) -> (a, r)
  | SNode(l,a,r) -> 
      let (minValue, deleted) = deleteSmallest l
      (minValue, SNode(deleted, a, r))

let rec delete x t = 
  match t with
  | SLeaf -> failwith "delete invariant - must delete a node"
  | SNode(SLeaf, a, SLeaf) when x = a -> SLeaf
  | SNode(SLeaf, a, SLeaf) -> t
  | SNode(l, a, SLeaf) when x = a -> l
  | SNode(l, a, SLeaf) -> SNode(delete x l, a, SLeaf)
  | SNode(SLeaf, a, r) when x = a -> r
  | SNode(SLeaf, a, r) -> SNode(SLeaf, a, delete x r)
  | SNode(l,a,r) when x < a -> SNode(delete x l, a, r)
  | SNode(l,a,r) when x > a -> SNode(l, a, delete x r)
  | SNode(l,a,r) -> let (aSuccessor, withDeleted) = deleteSmallest r
                    SNode(l, aSuccessor, withDeleted) 


let st0 = SNode(SLeaf, 0, SNode(SLeaf,2,SNode(SLeaf,4,SLeaf)))
let st1 = SNode(st0, 5, SNode(SLeaf, 7, SLeaf))
let st2 = SNode(SNode(SLeaf,1,SLeaf), 2, SNode(SNode(SNode(SLeaf, 3, SLeaf), 4, SNode(SNode(SLeaf, 5, SLeaf), 6, SLeaf)), 7, SNode(SLeaf, 8, SLeaf)))
let st3 = SNode(SNode(SLeaf, 3, SLeaf), 4, SNode(SNode(SLeaf, 5, SLeaf), 6, SLeaf))

//6.7

type Prop = 
  | Atom of string
  | Conj of Prop * Prop
  | Disj of Prop * Prop
  | Neg of Prop

let pt0 = Neg(Conj(Atom "p", Atom "q"))
let pt1 = Neg(Disj(Atom "p", Atom "q"))

// p and ~(q or r) = p and (~q and ~r)
let pt2 = Conj(Atom "p", Neg(Disj(Atom "q", Atom "r")))
let pt3 = Neg(Atom "p")
let pt4 = Conj(Atom "p", Atom "q")
let pt5 = Disj(Atom "p", Atom "q")

// ~( ~(p and q) or ~(r or s) ) 
//    = ~( (~p or ~q) or (~r and ~s)) 
//    = ~(~p or ~q) and ~(~r and ~s)
//    = (p and q) and (r or s)
//
// = ~~(p and q) and ~~(r or s)
let pt6 = Neg(Disj(Neg(Conj(Atom "p", Atom "q")), Neg(Disj(Atom "r", Atom "s"))))

let pt7 = Conj(Conj(Atom "p", Atom "q"), Atom "r")

let rec normalForm prop = 
  match prop with 
  | Neg(Neg(p)) -> normalForm p
  | Neg(Conj(p,q)) -> Disj(normalForm (Neg p), normalForm (Neg q))
  | Neg(Disj(p,q)) -> Conj(normalForm (Neg p), normalForm (Neg q))
  | Atom _ -> prop
  | Conj(p, q) -> Conj(normalForm p, normalForm q)
  | Disj(p, q) -> Disj(normalForm p, normalForm q)
  | Neg p -> Neg(normalForm p)

let rec propStringSimple prop = 
  match prop with
  | Atom p -> string p
  | Neg p -> "~" + propStringSimple p
  | Conj(p,q) -> "(" + propStringSimple p + " and " + propStringSimple q + ")"
  | Disj(p,q) -> "(" + propStringSimple p + " or " + propStringSimple q + ")"

let bracket f e = "(" + f e + ")"

let rec propStr prop =
  match prop with
  | Atom p -> string p
  | Neg p -> "~" + negArg(p)
  | Conj(p,q) -> conjArg(p) + " and " + conjArg(q)
  | Disj(p,q) -> disjArg(p) + " or " + disjArg(q)
and conjArg e = match e with | Disj _ -> bracket propStr e | _ -> propStr e
and disjArg e = match e with | Conj _ -> bracket propStr e | _ -> propStr e
and negArg e = match e with
                | Atom _ -> propStr e
                | Neg _ -> propStr e
                | _ -> bracket propStr e

let pt8 = Disj(Conj(Atom "a", Atom "b"), Atom "c")
let pt9 = Disj(Conj(Atom "a", Atom "b"), Atom "c")
let pt10 = Disj(pt8, Atom "d")

// ~(b or c) -> ~b and ~c
let pt11 = Neg(Disj(Atom "b", Atom "c"))

// a and (b or (d and e)) -> a and (b or d) and (b or e)
let pt12 = Conj(Atom "a", Disj(Atom "b", Conj(Atom "d", Atom "e")))

let literal = function 
  | Atom _ -> true
  | Neg _ -> true
  | _ -> false

let rec cnf prop = 
  match prop with 
  | Atom _ -> prop
  | Neg _ -> cnf prop
  | Disj(p, q) when literal(p) && literal(q) -> Disj(p, q)
  | Disj(p, Conj(q, r)) -> Conj(cnf (Disj(p, q)), cnf (Disj(p, r)))
  | Disj(Conj(p,q),r) -> Conj(cnf (Disj(p,r)), cnf (Disj(q,r)))
  | Disj(p,q) -> cnf (Disj(cnf p, cnf q))
  | Conj(p,q) -> Conj(cnf p, cnf q)
  
let toCnf prop = prop |> normalForm |> cnf
let prop2str prop = prop |> toCnf |> propStr

// a or b or ~a or ~b -> Disj(a, Disj(b, Disj(Neg(a), Neg(b))))
let pt13 = Disj(Atom "a", Disj(Atom "b", Disj(Neg(Atom "a"), Neg(Atom "b"))))


let rec accDisSymbols' (prop:Prop) ((s,ns):Set<string>*Set<string>) = 
  match prop with
  | Atom p -> (Set.add p s, ns)
  | Neg(Atom p) -> (s, Set.add p ns)
  | Disj(Atom p, q) -> accDisSymbols' q (Set.add p s, ns)
  | Disj(p, Atom q) -> accDisSymbols' p (Set.add q s, ns)
  | Disj(Neg(Atom p), q) -> accDisSymbols' q (s, Set.add p ns) 
  | Disj(p, Neg(Atom q)) -> accDisSymbols' p (s, Set.add q ns) 
  | _ -> failwith "accDisSymbols invariant : not a disjunction of literals"

let tautologyCheck prop = accDisSymbols' prop (Set.empty, Set.empty)

(*let rec tautologyCheck prop symbols = *)
  (*match prop with*)
  (*| Atom _ -> true*)
  (*| Neg p -> not (tautologyCheck p)*)
  (*| Disj(Atom p, Neg(Atom q)) -> p = q*)
  (*| Disj(Neg(Atom p), Atom q) -> p = q*)
  (*| Disj(Atom p, Disj q) ->   (tautologyCheck q)*)
