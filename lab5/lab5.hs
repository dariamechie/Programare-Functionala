--1)
propr :: Int->Int 
propr x
    | odd x = x*x
    | otherwise = 0 --sa l faca 0 pt ca sa adauge 0 la suma cand o facem la linia 11

lista :: [Int]->[Int]
lista ls = map propr ls

ex1 :: [Int]->Int
ex1 ls  = sum (lista ls)

--2)

ex2 :: [Bool]->Bool
ex2 ls = foldr (&&) True ls --verificam daca toate el sunt true

--3)

allVerifies0 :: (Int -> Bool) -> [Int] -> [Bool] --returneaza o lista de bool, dupa ce aplicam propr p => 0, daca nu satisface propr, 1 daca o satisface
allVerifies0 p ls = map p ls 

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies p ls = ex2 (allVerifies0 p ls)

--4) 
verif :: [Bool]->Bool
verif ls = foldr (||) False ls --Verif daca cel putin unul din ls e true => true daca e, false daca nu e

allVerifies04 :: (Int -> Bool) -> [Int] -> [Bool] --returneaza o lista de bool, dupa ce aplicam propr p => 0, daca nu satisface propr, 1 daca o satisface
allVerifies04 p ls = map p ls 

allVerifies4 :: (Int -> Bool) -> [Int] -> Bool
allVerifies4 p ls = verif (allVerifies0 p ls)

--5)

mapFoldr :: (a->b)->[a]->[b]
mapFoldr fct ls = foldr (\x lista0 -> fct x:lista0) [] ls

filterFoldr::(a->Bool)->[a]->[a]
filterFoldr f lista = foldr(\x lista01 -> if f x then x:lista01 else lista01) [] lista

--6)

functie::Int->Int->Int
functie rez x = rez*10+x

listToInt :: [Int] -> Int
listToInt ls = foldl functie 0 ls 

--7)
--a)

ex7 :: Char->String -> String
ex7 c s = filter (/= c) s -- /= inseamna diferit


