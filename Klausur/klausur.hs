{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda" #-}

foo' :: (a -> a -> b) -> (d -> a) -> d -> b
foo' f g x = f (g x) (g x)

-- k :: Bool -> Bool

bar' :: (a, b) -> a
bar' = fst

summe :: Integral a => a -> a
summe n
    | n == 1 = 1
    | otherwise = n + summe (n - 1)

summe' :: Integral a => a -> a
summe' n = s_ n 0
  where
    s_ n akk = if n == 1 then n + akk else s_ (n - 1) (n + akk)

summe'' :: Integral a => a -> a
summe'' = \n ->
    let s_ = \n akk -> if n == 1 then n + akk else s_ (n - 1) (akk + n)
     in s_ n 0

foo :: p -> Integer
foo = \x -> 1
bar :: Integer -> Integer
bar = \x -> if x == 0 then 1 else x * x

data BB a = L | K (BB a) a (BB a) deriving (Show)

pref :: BB a -> [a]
pref L = []
pref (K l w r) = w : pref l ++ pref r

inf :: BB a -> [a]
inf L = []
inf (K l w r) = inf l ++ [w] ++ inf r

w :: BB String
w = K (K (K L "10" L) "*" (K L "2" L)) "+" (K L "3" L)

fmapBB :: (a -> b) -> BB a -> BB b
fmapBB f L = L
fmapBB f (K l a r) = K (fmapBB f l) (f a) (fmapBB f r)

instance Functor BB where
    fmap :: (a -> b) -> BB a -> BB b
    fmap = fmapBB

absBetrag :: (Functor t, Num a) => t a -> t a
absBetrag = fmap abs

absBB :: BB Integer
absBB = let bb = K (K L (-1) L) 2 (K (K L 3 L) (-4) (K L 5 L)) in absBetrag bb

data Color = Red | Blue | Green | Pink | Orange | Yellow deriving (Show, Read, Eq, Enum)

data AstronautList = Astronaut String Color AstronautList | Sentinel

farbe :: Color -> AstronautList -> Bool
farbe color Sentinel = True
farbe color (Astronaut n c as)
    | color == c = False
    | otherwise = farbe color as

test :: AstronautList
test = Astronaut "Messi" Blue (Astronaut "Ronaldo" Red (Astronaut "Neymar" Yellow Sentinel))

showAL :: AstronautList -> [Char]
showAL as = "[ " ++ sAL as ++ " ]"
  where
    sAL Sentinel = ""
    sAL (Astronaut n c Sentinel) = concat [n, ":", show c]
    sAL (Astronaut n c as) = concat [n, ":", show c, " | ", sAL as]

appendOdds :: (Foldable f, Applicative f, Monoid (f a), Integral a) => a -> f a -> f a
appendOdds x m = if x `mod` 2 == 1 then pure x `mappend` m else m

odds :: (Foldable f, Applicative f, Monoid (f a), Integral a) => f a -> Int
odds ls = length ((foldr (\v l -> appendOdds v l) [] ls))

--odds' :: ( Foldable f, Applicative f, Monoid (f a), Integral a) => f a -> Int
--odds' = length . (foldr appendOdds mempty)


data Expr -- Constants
    = ConstI Int -- 23
    | ConstB Bool -- True
    -- Tuples
    | Tuple2 Expr Expr -- (e1 , e2)
    -- Operations
    | Add Expr Expr -- e1 + e2
    | Equal Expr Expr -- e1 == e2
    deriving (Show)

data Type
    = TInt
    | TBool
    | TTuple2 Type Type
    deriving (Eq, Show)

typecheck :: Expr -> Maybe Type
typecheck (ConstI e) = Just TInt
typecheck (ConstB e) = Just TBool
typecheck (Add e1 e2) = do
    t1 <- typecheck e1
    t2 <- typecheck e2
    if t1 == TInt && t2 == TInt then Just TInt else Nothing
typecheck (Equal e1 e2) = do
    t1 <- typecheck e1
    t2 <- typecheck e2
    if t1 == t2 then Just TBool else Nothing
typecheck (Tuple2 e1 e2) =
    typecheck e1
        >>= \t1 -> typecheck e2 >>= \t2 -> return (TTuple2 t1 t2)

data LL a = Elem a (LL a) | Empty

instance Functor LL where
    fmap :: (a -> b) -> LL a -> LL b
    fmap f Empty = Empty
    fmap f (Elem x xs) = Elem (f x) (fmap f xs)

instance Applicative LL where
    pure x = Elem x Empty
    (<*>) :: LL (a -> b) -> LL a -> LL b
    _ <*> Empty = Empty
    Empty <*> _ = Empty
    (Elem f fs) <*> (Elem x xs) = Elem (f x) (fs <*> xs) --Sinnvolle Implementierung

instance Integral a => Semigroup (LL a) where
    (<>) :: Integral a => LL a -> LL a -> LL a
    (Elem x Empty) <> ys = Elem x ys
    (Elem x xs) <> ys = Elem x (xs <> ys) 

instance Foldable LL where
    foldr _ akk Empty = akk
    foldr f akk (Elem x rest) = f x (foldr f akk rest) 

instance Integral a => Monoid (LL a) where
    mempty = Empty
    