
data Expr = Const Int -- integer constant
          | Expr :+: Expr -- addition
          | Expr :*: Expr -- multiplication
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)

--1.1)
instance Show Expr where 
  show :: Expr -> String
  show (Const i) = show i
  show (e1 :+: e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (e1 :*: e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    
--1.2)
evalExp :: Expr -> Int
evalExp (Const c)= c
evalExp (exp1 :+: exp2) = evalExp exp1 + evalExp exp2
evalExp (exp1 :*: exp2) = evalExp exp1 * evalExp exp2

--1.3)
evalArb :: Tree -> Int
evalArb (Lf a) = a
evalArb(Node Add e1 e2) = evalArb e1 + evalArb e2 
evalArb(Node Mult e1 e2) = evalArb e1 * evalArb e2 


-- arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
-- arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
-- arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
-- arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

-- test21 = evalArb arb1 == 6
-- test22 = evalArb arb2 == 14
-- test23 = evalArb arb3 == 13
-- test24 = evalArb arb4 == 16

--1.4)
expToArb :: Expr -> Tree
expToArb (Const c) = Lf c
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)


class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value

  
  keys c = map fst (toList c)
  values c = map snd (toList c)

  fromList [] = empty
  fromList ((cheie,valoare):xs) = insert cheie valoare (fromList xs) --inseram cheia si valoarea in lista 

--2.2)
newtype PairList k v
  = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
 empty :: PairList key value
 empty = PairList []
 toList :: PairList key value -> [(key,value)]
 toList = getPairList
 insert k v (PairList ls) = PairList $ (k,v):filter((/=k).fst) ls
 clookup k (PairList l) = lookup k l
 --delete k (PairList l)=PairList$ filter((/=k).fst) l
 



data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
    empty = Empty
    singleton k v = BNode Empty k (Just v) Empty
    insert k v Empty = singleton k v
    insert k v (BNode a cheie valoare b) = if k <= cheie then (BNode (insert k v a) cheie valoare b)
                                          else  (BNode a cheie valoare (insert k v b))
    clookup k Empty = Nothing
    clookup k (BNode a cheie valoare b) = if k == cheie then valoare
                                        else if k < cheie then (clookup k a)
                                            else (clookup k b)

  
    toList Empty = []
    toList (BNode a cheie valoare b) = case valoare of 
                                        Nothing -> (toList a) ++ (toList b) 
                                        Just x -> (toList a) ++ [(cheie, x)] ++ (toList b) 