
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


let rec toString = function
  | Const x -> string x
  | X -> "x"
  | Add(e1,e2) -> "(" + (toString e1) + ")" + " + " + "(" + (toString e2) + ")"
  | Sub(e1,e2) -> "(" + (toString e1) + ")" + " - " + "(" + (toString e2) + ")"
  | Mul(e1,e2) -> "(" + (toString e1) + ")" + " * " + "(" + (toString e2) + ")"
  | Div(e1,e2) -> "(" + (toString e1) + ")" + " / " + "(" + (toString e2) + ")"
  | Sin e -> "sin(" + (toString e) + ")"
  | Cos e -> "cos(" + (toString e) + ")"
  | Log e -> "log(" + (toString e) + ")"
  | Exp e -> "exp(" + (toString e) + ")"
  

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

// 6.2
let e1 = Add(X, Const 7.0)
let e2 = Mul(Add(X, Const 7.0), Sub(X, Const 5.0))
let rec postfixStr = function
  | Add(e1,e2) -> "+ " + "(" + (postfixStr e1) + ") "  + "(" + (postfixStr e2) + ")"
  | Sub(e1,e2) -> "- " + "(" + (postfixStr e1) + ") "  + "(" + (postfixStr e2) + ")"
  | Mul(e1,e2) -> "* " + "(" + (postfixStr e1) + ") "  + "(" + (postfixStr e2) + ")"
  | Div(e1,e2) -> "/ " + "(" + (postfixStr e1) + ") "  + "(" + (postfixStr e2) + ")"
  | e -> toString e

// 6.3
//

