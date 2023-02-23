{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}
newtype Identity a = Identity a

data Pair a = Pair a a

data Constant a b = Constant b

data Two a b = Two a b

data Three a b c = Three a b c

data Three' a b = Three' a b b


data Four a b c d = Four a b c d

data Four'' a b = Four'' a a a b

data Quant a b = Finance | Desk a | Bloor b


data LiftItOut f a = LiftItOut (f a)

data Parappa f g a = DaWrappa (f a) (g a)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

data Notorious g o a t = Notorious (g o) (g a) (g t)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

data TalkToMe a = Halt | Print String a | Read (String -> a)


----------------------------------------------------------------------------------------------------------------------

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity x) = (Identity (f x))

instance Functor Pair where 
    fmap :: (a->b) -> Pair a -> Pair b
    fmap f (Pair a b) = Pair (f a) (f b)

--data Constant a b = Constant b
--     Constant `x` :: Constant Int Char
--     Constant `x` :: Constant Bool Char
    
instance Functor (Constant a) where
    fmap f (Constant b) = Constant (f b)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Functor (Three a b) where
    fmap f(Three a b c) = Three a b (f c)

instance Functor (Three' a) where
    fmap f(Three' a b b1) = Three' a (f b) (f b1)

instance Functor (Four a b c) where
    fmap f(Four a b c d) = Four a b c (f d)

instance Functor (Four'' a) where
    fmap f(Four'' a1 a2 a3 b) = Four'' a1 a2 a3 (f b)

instance Functor (Quant a) where
    fmap f Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

-- aToB e functie

instance Functor f => Functor (LiftItOut f) where
    fmap aToB (LiftItOut fA) = LiftItOut (fmap aToB fA)
    
--data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g ) where
    fmap aToB (DaWrappa fA gA) = DaWrappa (fmap aToB fA) (fmap aToB gA)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
    fmap aToB (IgnoringSomething fA gB) = IgnoringSomething fA (fmap aToB gB)

instance Functor g => Functor (Notorious g o a) where
    fmap fct (Notorious gO gA gT) = Notorious gO gA (fmap fct gT)

