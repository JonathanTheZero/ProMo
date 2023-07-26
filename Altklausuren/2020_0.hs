{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant lambda" #-}

_f :: (a -> b -> c) -> (d -> b) -> (d -> a) -> d -> c
_f g h j x = g (j x) (h x)

z :: (a -> a) -> a -> a -> a
z a b = a

-- m :: Int -> Int

magnitude :: Floating a => [a] -> a
magnitude = sqrt . mag
  where
    mag [] = 0
    mag (x : xs) = x ^ 2 + mag xs

magnitude' :: Floating a => [a] -> a
magnitude' = sqrt . mag 0
  where
    mag akk xs = if null xs then akk else mag (head xs ^ 2 + akk) (tail xs)

magnitude'' :: Floating a => [a] -> a
magnitude'' = sqrt . foldr (\a r -> r + a * a) 0

wurzel :: Double -> Double
wurzel = \x -> sqrt x
index :: p -> Integer
index = \n -> -4
f :: Double -> Double
f = \n -> if n < 0 then n * n else wurzel n

{- index (f (f (index 0))) applikativ
index (f (f (index 0)))
index (f (f ((\n -> -4) 0)))
index (f (f (-4)))
index (f ((\n -> if n < 0 then n * n else wurzel n) (-4)))
index (f (if -4 < 0 then -4 * -4 else wurzel -4))
index (f (if True then (-4) * (-4) else wurzel (-4)))
index (f ((-4) * (-4)))
index (f (16))
index ((\n -> if n < 0 then n * n else wurzel n) 16)
index (if 16 < 0 then 16 * 16 else wurzel 16)
index (if False then 16 * 16 else wurzel 16)
index (wurzel 16)
index ((\x -> sqrt x) 16)
index (sqrt 16)
index 4
(\n -> -4) 4
-4
-}

{- index (f (f (index 0))) normal
index (f (f (index 0)))
(index = \n -> -4) (f (f (index 0)))
-4
-}

{-  f (index 0) lazy
f (index 0)
let a = index 0 in f a
    f a
    (\n -> if n < 0 then n * n else wurzel n) a
    if a < 0 then a * a else wurzel a
        a
        index 0
        (index = \n -> -4) 0
        -4
    if -4 < 0 then (-4) * (-4) else wurzel (-4)
    if True then (-4) * (-4) else wurzel (-4)
    (-4) * (-4)
    16
16
-}

data BB a = L | K (BB a) a (BB a)

w = K (K (K L "(*)" L) "<$>" (K L "[3]" L)) "<*>" (K L "[2]" L)

pref :: BB a -> [a]
pref L = []
pref (K l w r) = w : pref l ++ pref r

inf :: BB a -> [a]
inf L = []
inf (K l w r) = inf l ++ [w] ++ inf r

-- inf w = (*) <$> [3] <*> [2] = [6]

foldlBB :: (b -> a -> b) -> b -> BB a -> b
foldlBB f b L = b
foldlBB f b (K l a r) = foldlBB f (f (foldlBB f b l) a) r

-- i.d.R ist es nicht möglich Foldable nur mit linksfaltung zu Implementierung
-- da diese nicht mit unendlichen Datenstrukturen umgehen kann
-- wenn man jedoch nur endliche Bäume berücksichtigt, ist es möglich diese wie folgt zu definieren

instance Foldable BB where
    foldl :: (b -> a -> b) -> b -> BB a -> b
    foldl = foldlBB
    foldr :: (a -> b -> b) -> b -> BB a -> b
    foldr f a bs = foldlBB (\g b x -> g (f b x)) id bs a


data Map a b = MapElem a b (Map a b) | MapSentinel

tuplesToMap :: [(a, b)] -> Map a b
tuplesToMap [] = MapSentinel
tuplesToMap ((a, b) : xs) = MapElem a b (tuplesToMap xs)

showMap :: (Show a1, Show a2) => Map a1 a2 -> [Char]
showMap m = "{ " ++ showMap' m ++ " }"
  where
    showMap' MapSentinel = ""
    showMap' (MapElem a b MapSentinel) = concat [show a, " -> ", show b]
    showMap' (MapElem a b rest) = concat [show a, " ->", show b, ", ", showMap' rest]

appendEven :: (Applicative f, Monoid (f a), Integral a) => a -> f a -> f a
appendEven x m = if x `mod` 2 == 0 then pure x `mappend` m else m

even' :: (Foldable t, Applicative t, Monoid (t a), Integral a) => t a -> t a
even' = foldr (\l r -> appendEven l r) mempty

even'' :: (Foldable t, Applicative t, Monoid (t a), Integral a) => t a -> t a
even'' = foldr appendEven mempty

data Expr
    = Num Double
    | Add Expr Expr
    | Var Char
    | Sqrt Expr
    deriving (Show)

type Env = [(Char, Double)]

evalDo :: Env -> Expr -> Maybe Double
evalDo _ (Num a) = return a
evalDo e (Var c) = getvar e c
evalDo e (Add a b) = do
    a' <- evalDo e a
    b' <- evalDo e b
    Just (a' + b')
evalDo e (Sqrt a) = do
    a' <- evalDo e a
    if a' < 0 then Nothing else return (sqrt a')

getvar :: Env -> Char -> Maybe Double
getvar = flip lookup
