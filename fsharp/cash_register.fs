type ArticleCode = string
type ArticleName = string

type Price = int
type Register = (ArticleCode * (ArticleName * Price)) list

let reg = [("a1", ("cheese", 25));
           ("a2", ("herring", 4));
           ("a3", ("soft drink", 5))]
type NoPieces = int;;
type Item = NoPieces * ArticleCode
type Purchase = Item list

let pur = [(3, "a2"); (1, "a1")]

let rec findArticle ac = function
  | (ac', adesc)::_ when ac=ac' -> adesc
  | _::reg                      -> findArticle ac reg
  | _ -> failwith(ac + " unknown article")

let rec makeBill reg = function
  | [] -> ([], 0)
  | (np,ac)::pur -> let (aname,aprice) = findArticle ac reg
                    let tprice = np*aprice
                    let (billt1, sumt1) = makeBill reg pur
                    ((np,aname,tprice)::billt1,tprice+sumt1)


