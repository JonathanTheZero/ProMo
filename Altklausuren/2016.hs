{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant guard" #-}

_f :: Int -> b -> (Int, b)
_f x y = (3 * x :: Int, y)

_b :: Integer -> Integer -> Integer
_b = (\x y -> (x + y) `mod` y)

_c :: [a] -> [a] -> [a]
_c = (\x -> (\y -> x ++ y))

_d :: a -> [a]
_d = (\x -> [x])

produkt :: Integral a => a -> a
produkt 1 = 1
produkt n
    | n <= 0 = 1
    | otherwise = n * produkt (n - 1)

produktEnd :: Integral a => a -> a
produktEnd n = _prod n 1
  where
    _prod 1 akk = akk
    _prod n akk
        | n <= 0 = akk
        | otherwise = _prod (n - 1) (n * akk)

f :: [a] -> [a]
g :: p -> Integer
f = tail . tail
g = (\x -> 42)
h :: Integer -> Integer
h = (\x -> x * x)

{-
g (h 3) applikativ ausgwertet
g (h 3)
g ((\x -> x * x) 3)
g (3 * 3)
g 9
(\x -> 42) 9
42
-}

{-
g (h 3) normal ausgewertet
g (h 3)
(\x -> 42) (h 3)
42
-}

{-
h (h 3) lazy/verzögert ausgwertet
let a = h 3 in h a
    h a
    (\x -> x * x) a
    a * a
        a
        h 3
        let b = 3 in h b
            (\x -> x * x) b
            b * b
            3 * 3
            9
        9
        9
    9 * 9
    81
81
-}

{-
f [1, 2, 3] lazy/verzögert ausgewertet
f [1, 2, 3]
let a = [1, 2, 3] in f a
    f a
    (tail . tail) a
    let b = tail a in tail b
        tail a
        tail [1, 2, 3]
        [2, 3]
    tail [2, 3]
    [3]
[3]
-}

data BB a = L | K (BB a) a (BB a)
tree :: BB Integer
tree = K (K (K L 1 L) 2 (K L 3 L)) 4 (K (K L 5 L) 4 (K L 6 L))

suche :: Eq a => a -> BB a -> Bool
suche el L = False
suche el (K l w r)
    | el == w = True
    | otherwise = suche el l || suche el r

tief :: b -> (b -> a -> b -> b) -> BB a -> b
tief fL fK L = fL
tief fL fK (K linkerBaum w rechterBaum) =
    fK (tief fL fK linkerBaum) w (tief fL fK rechterBaum)

-- bruh, Klausur einfach kaputt

anzahlKnoten :: BB a -> Int
anzahlKnoten = tief 0 (\left w right -> left + right + 1)

baumTiefe :: BB a -> Int
baumTiefe = tief 0 (\left w right -> (max left right) + 1)

istIn :: Eq a => a -> BB a -> Bool
istIn wert = tief False (\left w right -> w == wert || left || right)

data Vorname = EV String | MV String (Vorname)
v1 :: Vorname
v1 = MV "Friedrich" (EV "Wilhelm")
v2 :: Vorname
v2 = EV "Herbert"
v3 :: Vorname
v3 = MV "Karl" (MV "Johannes" (MV "Heinrich" (EV "Harald")))

vDruck :: Vorname -> String
vDruck (EV n) = n
vDruck (MV n n2) = n ++ " " ++ vDruck n2

data Name1 = N1 Vorname String
n1 :: Name1
n1 = N1 v1 "Leopold"
druckt1 :: Name1 -> String
druckt1 (N1 vn nn) = vDruck vn ++ " " ++ nn

data Name2 = N2 String String String
n2 :: Name2
n2 = N2 "Volker" "Fernandez" "di Maria"
druckt2 :: Name2 -> String
druckt2 (N2 a b c) = a ++ " " ++ b ++ " " ++ c

class Name a where
    druckt :: a -> String

instance Name Name1 where
    druckt :: Name1 -> String
    druckt = druckt1

instance Name Name2 where
    druckt :: Name2 -> String
    druckt = druckt2

data Paar a = P a a

plus :: Num a => Paar a -> Paar a -> Paar a
plus (P x1 y1) (P x2 y2) = P (x1 + x2) (y1 + y2)

instance Integral a => Semigroup (Paar a) where
    (<>) :: Integral a => Paar a -> Paar a -> Paar a
    (<>) = plus

instance Integral a => Monoid (Paar a) where
    mempty :: Integral a => Paar a
    mempty = P 0 0

minus :: Maybe Int -> Maybe Int -> Maybe Int
minus Nothing _ = Nothing
minus _ Nothing = Nothing
minus (Just n) _ | n < 0 = Nothing
minus _ (Just n) | n < 0 = Nothing
minus (Just n1) (Just n2) | n1 < n2 = Nothing
minus (Just n1) (Just n2) | otherwise = Just (n1 - n2)