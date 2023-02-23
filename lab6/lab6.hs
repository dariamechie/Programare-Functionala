data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]


--1) 
--a)

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia(Portocala "Tarocco" _ ) = True
ePortocalaDeSicilia(Portocala "Moro" _ ) = True
ePortocalaDeSicilia(Portocala "Sanguinello" _ ) = True
ePortocalaDeSicilia(Portocala _ _ ) = False
ePortocalaDeSicilia(Mar _ _ ) = False

--var 2
-- ePortocalaDeSicilia(Portocala soi _ ) = soi `elem` ["Tarocco" , "Moro" , "Sanguinello"]
-- ePortocalaDeSicilia _ = False



test_ePortocalaDeSicilia1 =
    ePortocalaDeSicilia (Portocala "Moro" 12) == True
test_ePortocalaDeSicilia2 =
    ePortocalaDeSicilia (Mar "Ionatan" True) == False

--b)

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia x = sum[nr_felii | (Portocala soi nr_felii) <-x , ePortocalaDeSicilia(Portocala soi nr_felii) == True]


test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

--c)

eMarVierme :: Fruct -> Bool
eMarVierme (Mar _ True) = True
eMarVierme _ = False

nrMereViermi :: [Fruct] -> Int
nrMereViermi list = length [x | x <- list, eMarVierme x == True] --bagam in lista doar merele care sunt true

test_nrMereViermi = nrMereViermi listaFructe == 2

--2)

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

--a)
vorbeste :: Animal -> String
vorbeste(Pisica _) = "Meow!"
vorbeste(Caine _ _) = "Woof!"

--b)
rasa :: Animal -> Maybe String
rasa(Caine _ rasa_caine) = Just rasa_caine
rasa _ = Nothing

--3)

data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

--a)
verifica :: Matrice -> Int -> Bool
verifica (M mat) n = foldr (&&) True [sum linie==n | L linie <- mat] 
--verifica (M mat) n = and [sum l==n | L linie <- mat] 


test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True

--b)
doarPozN :: Matrice -> Int -> Bool
doarPozN (M mat) n = foldr (&&) True [ all(>0) linie | L linie <- mat, length linie==n] 

testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True

testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False

--c)

linie:: Linie->[Int]
linie(L l)=l

corect :: Matrice -> Bool
corect(M []) = True
corect(M [L _])=True
corect(M(L l1 : L l2 : xs))
    |length l1 == length l2 = corect(M(L l2:xs))
    |otherwise = False

testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True