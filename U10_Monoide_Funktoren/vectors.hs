{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

data Triple a = Triple a a a deriving (Eq)

instance (Show a) => Show (Triple a) where
    show :: Show a => Triple a -> String
    show (Triple x y z) = show x ++ " | " ++ show y ++ " | " ++ show z

instance Functor Triple where
    fmap :: (a -> b) -> Triple a -> Triple b
    fmap f (Triple a b c) = Triple (f a) (f b) (f c)

instance Applicative Triple where
    pure :: a -> Triple a
    pure a = Triple a a a
    (<*>) :: Triple (a -> b) -> Triple a -> Triple b
    (Triple a b c) <*> (Triple d e f) = Triple (a d) (b e) (c f)

tfst :: Triple a -> a
tfst (Triple t _ _) = t
tsnd :: Triple a -> a
tsnd (Triple _ t _) = t
ttrd :: Triple a -> a
ttrd (Triple _ _ t) = t

tripleFromList :: [a] -> Triple a
tripleFromList list
    | length list == 1 = Triple (head list) (head list) (head list)
    | length list == 2 = Triple (head list) (list !! 2) (list !! 2)
    | otherwise = Triple (head list) (list !! 2) (list !! 3)

tripleFromList' :: [a] -> Triple a
tripleFromList' (x:y:z:_) = Triple x y z
tripleFromList' _ = error "Unable to convert to Triple"

tripleToList :: Triple a -> [a]
tripleToList t = [tfst t, tsnd t, ttrd t]

x :: Num a => Triple a -> Triple a -> Triple a
(Triple a b c) `x` (Triple d e f) = Triple (b * f - c * e) (c * d - a * f) (a * e - b * d)

scaMult :: Num a => a -> Triple a -> Triple a
scaMult s = fmap (* s)

o :: Num a => Triple a -> Triple a -> a
(Triple a b c) `o` (Triple d e f) = a * d + b * e + c * f

o' :: Num a => Triple a -> Triple a -> a
o' t1 t2 = sum (tripleToList ((*) <$> t1 <*> t2))

tripleAdd :: Num a => Triple a -> Triple a -> Triple a
-- tripleAdd (Triple a b c) (Triple d e f) = Triple (a + d) (b + e) (c + f)
tripleAdd t1 t2 = (fmap (+)) t1 <*> t2

tripleSub :: Num a => Triple a -> Triple a -> Triple a
tripleSub t1 t2 = (fmap (-)) t1 <*> t2

tlength :: Floating a => Triple a -> a
tlength (Triple a b c) = sqrt (a ^ 2 + b ^ 2 + c ^ 2)