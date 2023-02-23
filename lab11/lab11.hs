{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

Just length <*> Just "world"

Just (++" world") <*> Just "hello,"
pure (+) <*> Just 3 <*> Just 5
pure (+) <*> Just 3 <*> Nothing
(++) <$> ["ha","heh"] <*> ["?","!"]
-}
data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

-- fmap f [] = []
-- fmap f (x:xs) = f x :fmap f xs
instance Functor List where
    fmap f Nil = Nil 
    fmap f (Cons a b)= Cons (f a) (fmap f b)

app :: List a -> List a -> List a
app Nil ls = ls
app (Cons a as) ls = Cons a (app as ls)

instance Applicative List where
    pure :: a -> List a
    pure a = Cons a Nil

    (<*>) :: List (a -> b) -> List a -> List b
    (<*>) Nil ls = Nil --Nil <*> ls
    (<*>) (Cons a as) ls = (fmap a ls) `app` ((<*>) as ls)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

data Cow = Cow {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative x 
    |x<0 = Nothing
    |otherwise = Just x

test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing 
test23 = noNegative 5 == Just 5 
--b)
cowFromString :: String -> Int -> Int -> Maybe Cow
-- cowFromString name age weight
--     |noEmpty name == Nothing  =  Nothing
--     |noNegative age == Nothing  =  Nothing
--     |noNegative weight == Nothing  = Nothing
--     |otherwise = Just Cow {name, age, weight}


test24 = cowFromString "Milka" 5 100 == Just (Cow {name = "Milka", age = 5, weight = 100})

--c)

 
cowFromString name age weight = 
    fmap Cow (noEmpty name) <*> noNegative age <*> noNegative weight


newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength x str
    |x< length (str) = Nothing
    |otherwise = Just str

test31 = validateLength 5 "abc" == Just "abc"

mkName :: String -> Maybe Name
mkName str 
    |validateLength 25 str == Nothing = Nothing
    |otherwise = Just(Name str)


mkAddress :: String -> Maybe Address
mkAddress str 
    |validateLength 100 str == Nothing = Nothing
    |otherwise = Just(Address str)

test32 = mkName "Gigel" ==  Just (Name "Gigel")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

--mkPerson :: String -> String -> Maybe Person
--mkPerson str1 str2 = fmap Person (validateLength 25 str1) <*> validateLength 100 str2

    -- |validateLength 25 str1 == Nothing = Nothing
    -- |validateLength 100 str2 == Nothing = Nothing
    -- |otherwise = Just(Person (Name str1) (Address str2))

test34 = mkPerson "Gigel" "Str Academiei" == Just (Person (Name "Gigel") (Address "Str Academiei"))
