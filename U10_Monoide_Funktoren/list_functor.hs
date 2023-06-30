{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

data List a = Nil | Cons a (List a)

conc :: List a -> List a -> List a
conc (Cons x Nil) l2 = Cons x l2
conc (Cons x l) l2 = Cons x (conc l l2)

test :: List Integer
test = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f Nil = Nil
    fmap f (Cons val l) = Cons (f val) (fmap f l)

instance Applicative List where
    (<*>) :: List (a -> b) -> List a -> List b
    _ <*> Nil = Nil
    Nil <*> _ = Nil
    --(Cons f fs) <*> (Cons x xs) = (pure (f x)) `conc` (fs <*> xs) --Implementierung nach Aufgabenstellung
    (Cons f fs) <*> xs = fmap f xs `conc` (fs <*> xs) --Sinnvolle Implementierung
    pure :: a -> List a
    pure x = Cons x Nil

instance (Show a) => Show (List a) where
    show :: Show a => List a -> String
    show (Cons val Nil) = "[" ++ show val ++ "]"
    show (Cons val l) = "[" ++ show val ++ "," ++ _show l
      where
        _show (Cons val Nil) = show val ++ "]"
        _show (Cons val l) = show val ++ "," ++ _show l

negateFunctor :: (Functor f, Num b) => f b -> f b
negateFunctor = fmap (* (-1))

lzipWith :: (a -> b -> c) -> List a -> List b -> List c
lzipWith _ Nil _ = Nil
lzipWith _ _ Nil = Nil
lzipWith f a b = (fmap f a) <*> b