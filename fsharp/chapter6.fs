
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

