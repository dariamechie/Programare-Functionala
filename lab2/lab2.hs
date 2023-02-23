polly2 :: Double -> Double -> Double -> Double -> Double
polly2 a b c x = a*x*x+b*x+c

eeny :: Integer -> String
eeny x = if even x then "eeny"
    else "weeny"
fizzbuzz :: Integer -> String
fizzbuzz x = if mod x 3==0 && mod x 5 == 0
    then "FizzBuzz"
    else if mod x 3==0
        then "Fizz"
    else if mod x 5==0
        then "Buzz"
    else ""
fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
tribonacci :: Integer -> Integer
tribonacci n 
    | n==0  =0
    | n==1  =1
    | n==2  =1
    | n==3  =2
    | otherwise = tribonacci(n-1)+tribonacci(n-2)+tribonacci(n-3)
tribonacci2 :: Integer -> Integer
tribonacci2 1 = 1
tribonacci2 2 = 1
tribonacci2 3 = 2
tribonacci2 n = tribonacci(n-1)+tribonacci(n-2)+tribonacci(n-3)

binomial :: Integer -> Integer -> Integer
binomial n 0 = 1
binomial 0 k = 0
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

verifL :: [Int] -> Bool
verifL ls =  even (length ls)
takefinal :: [Int] -> Int -> [Int]
takefinal ls n = if(length ls)>n then drop(length ls-n) ls
                 else ls
--drop(n) sterge primele n nr din lista

remove :: [a]-> Int -> [a]
remove ls n=take n ls ++ drop(n+1) ls 

-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
 | even h    = h `div` 2 : t'
 | otherwise = t'
 where t' = semiPareRec t

myreplicate:: Int->Int->[Int]
myreplicate n v
    |n>0 = v:ls --adaugam la inceputul listei ls pe v
    |otherwise = []
    where ls=myreplicate(n-1)v --de (n-1) ori

sumImp :: [Int] -> Int

--a)
--sumImp ls 
  --  |length ls>0 = mod(head ls)2+suma
   -- |otherwise = 0
   -- where suma=sumImp(tail ls)

--b)
sumImp []=0
sumImp(p:u)
    | even p=sumImp(u)
    | otherwise = p+sumImp(u)


totalLen :: [String] -> Int
totalLen []=0
totalLen(p:u)
    |head p=='A' = length(p) + totalLen(u)
    |otherwise = totalLen(u)