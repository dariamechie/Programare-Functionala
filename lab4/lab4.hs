import Data.List


--1)
factori:: Int->[Int]
factori x = [y | y<-[1..x], x `mod` y==0]

--2)
prim:: Int -> Bool
prim x = factori x == [1,x]

--3)
numerePrime:: Int -> [Int]
numerePrime n = [x |x<-[2..n], prim x]

--4)
myzip3:: [a]->[b]->[c]->[(a,b,c)]
myzip3 x y [] = []
myzip3 x [] z = []
myzip3 [] y z = []
myzip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : myzip3 xs ys zs

-- map :: (a -> b) -> [a] -> [b]
-- map f xs =[f x | x <- xs]


--5)
firstEl:: [(a,b)]->[a]
firstEl = map fst 

--6)
sumList:: [[Int]]->[Int]
sumList=map sum

--7)

propr :: Int->Int 
propr x
    | odd x = x*2
    | otherwise = x `div` 2

prel2::[Int]->[Int]
prel2 = map propr 

--9)
propr1 :: Int->Int 
propr1 x
    | odd x = x*x
    | otherwise = x

ex9::[Int]->[Int]
ex9=map propr1

--10)
getSquares::[Int]->[Int]     --[7,8,67]->[(0,7), (1,8), (2,67)]
getSquares l = map((^2).snd) (filter(odd.fst) (zip [0..] l)

sqrOdd :: [Int] -> [Int]
sqrOdd l = map (^ 2) (filter odd l)

--11)

ex11::[[String]] -> [[String]]
ex11 = map(filter(`elem` "aeiouAEIOU"))


myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (h:t)
    |f h = h:myfilter f t
    |otherwise = myfilter f t


mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (h:t) = (f h):mymap f t
