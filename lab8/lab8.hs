import Text.Parsec (parse)
import System.Win32 (xBUTTON1, SECURITY_ATTRIBUTES (bInheritHandle))

data Punct = Pt [Int]

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
 	    toArb :: a -> Arb
	    fromArb :: Arb -> a

--1) a)
instance Show Punct where 
  show(Pt l)="(" ++ parse l ++ ")"
   where 
    parse [] = ""
    parse [x] = show x
    parse (x:xs) = show x ++ "," ++ parse xs


--b)
instance ToFromArb Punct where
  toArb :: Punct -> Arb
  toArb (Pt []) = Vid
  toArb (Pt (x:xs)) = N(F x) (toArb (Pt xs))

  fromArb::Arb -> Punct
  fromArb Vid = Pt []
  fromArb (F x) = Pt [x] 
  fromArb (N l r) = Pt(l1 ++ l2)
    where 
      Pt l1 = fromArb l
      Pt l2 = fromArb r

-- Pt [1,2,3]
-- (1, 2, 3)

-- Pt []
-- ()

-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
--  (1,2,3)

{-
instance ToFromArb Punct where
    toArb(Pt []) = Vid
    toArb(Pt (x:xs)) = N(F x) (toArb (Pt xs))

    fromArb Vid = Pt []
    fromArb (F x)= Pt [x]
    fromArb (N (F left) (F right)) = Pt[left, right]
-}

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

-- ghci> pi
-- 3.141592653589793

--2) a)
instance GeoOps Geo where
  perimeter(Square a) = 4*a
  perimeter(Rectangle a b) = 2*(a+b)
  perimeter (Circle a) = 2*pi*a

  area (Square a) = a*a
  area (Rectangle a b) = a*b
  area (Circle a) = pi*a*a

--b)
instance (Floating a, Eq a) => Eq(Geo a) where
  a==b = perimeter(a) == perimeter (b)