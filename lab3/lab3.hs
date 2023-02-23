import Data.Char
import Data.List 

--1)
isVowel :: Char -> Bool
isVowel c = elem c "aeiouAeiou"

countVowels :: [Char] -> Int
countVowels []=0
countVowels (x:xs)
    | isVowel(x) == True= 1+countVowels(xs)
    | otherwise = countVowels(xs)

nrVocale :: [String] -> Int 
nrVocale[]=0
nrVocale(x:xs)
    | x==reverse(x)=countVowels(x)+nrVocale(xs)
    | otherwise=nrVocale(xs) 

--2)
adaugaPar:: Int -> [Int] -> [Int]
adaugaPar n [] = []
adaugaPar n(x:xs)
    |even x = x:n:adaugaPar n xs
    |otherwise= x:adaugaPar n xs 

--3) 
divizori :: Int -> [Int]
divizori x = [y | y<-[1..x], x`mod`y==0] 

--4)


--5)

--a)

intIntervalRec :: Int -> Int -> [Int] ->[Int]
intIntervalRec st dr [] = []
intIntervalRec st dr (x:xs)
    | x <= dr && x>=st = x:intIntervalRec st dr xs
    | otherwise = intIntervalRec st dr xs

--b)

intIntervalCmp :: Int -> Int -> [Int] ->[Int]
intIntervalCmp st dr l=[a | a<-l, a>=st, a<=dr]

--6)
--a)

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (x:xs)
     | x > 0 = 1 + pozitiveRec xs
     | otherwise = pozitiveRec xs
--b)
pozitiveComp :: [Int] -> Int
pozitiveComp lista = length [x | x<- lista, x>0]

--7)

index:: Int -> [Int] -> [Int]
index p [] = []
index p (x:xs)
    |odd x=p:index (p+1) xs
    |otherwise = index (p+1) xs


pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec ls=index 0 ls

--b)

pozitiiImpareCmp :: [Int] -> [Int]
pozitiiImpareCmp ls =[poz | (poz,x)<-zip[0..] ls, odd x]


produs :: String -> Int
produs "" = 1
produs (x:xs)
    |isDigit x == True = digitToInt x * produs xs
    |otherwise = produs xs

produs1:: String -> Int
produs1 ls = product [digitToInt x| x<- ls, isDigit x == True]