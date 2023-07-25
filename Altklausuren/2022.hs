{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use sum" #-}

f :: (a -> b -> c) -> (d -> b) -> (d -> a) -> d -> c
f g h j x = g (j x) (h x)

k :: Int
k = 10

c :: a -> b -> a
c = const

produkt :: Integral a => a -> a
produkt n | n <= 1 = 1
produkt n | otherwise = n * produkt (n - 1)

produkt' :: Integral a => a -> a -- endrekurisv
produkt' n = prod n 1
  where
    prod n akk =
        if n == 1
            then akk
            else prod (n - 1) (akk * n)

produkt'' :: Integral a => a -> a -- Lambda
produkt'' = \n ->
    let prod = \n akk -> if n == 1 then akk else prod (n - 1) (akk * n)
     in prod n 1

-- A3 a) ist normal
-- A3 b) ist index (f (f ((\n -> (-4)) 0)))
-- A3 c) (\n -> if n<0 then sqr n else n) (3+1) zu if (3+1)<0 then sqr (3+1) else (3+1))

data BB a = L | K (BB a) a (BB a)

pref :: BB a -> [a]
pref L = []
pref (K l w r) = w : pref l ++ pref r

inf :: BB a -> [a]
inf L = []
inf (K l w r) = inf l ++ [w] ++ inf r

w :: BB String
w =
    K
        ( K
            (K L "12" L)
            "*"
            (K L "2" L)
        )
        "-"
        (K L "4" L)

foldrBB :: (a -> b -> b) -> b -> BB a -> b
foldrBB f b L = b
foldrBB f b (K l a r) = foldrBB f (f a (foldrBB f b r)) l

test :: [Char]
test = foldrBB (\x xs -> concat ["(", show x, xs, ")"]) "" (K (K L 2 L) 1 (K (K L 4 L) 3 L))

instance Foldable BB where
    foldr :: (a -> b -> b) -> b -> BB a -> b
    foldr = foldrBB

summiere :: Foldable t => t Int -> Int
summiere = foldr (+) 0

sumBB :: Int
sumBB = let b = K (K L 2 L) 1 (K (K L 4 L) 3 L) in summiere b

data LinkedList a = ListElem a (LinkedList a) | ListSentinel

listToLL :: [a] -> LinkedList a
listToLL [] = ListSentinel
listToLL (x : xs) = ListElem x (listToLL xs)

showLL :: Show a => LinkedList a -> String
showLL ListSentinel = ""
showLL (ListElem a ListSentinel) = show a
showLL (ListElem a rest) = concat [show a, ">>>", showLL rest]

append2 :: (Applicative f, Monoid (f a)) => f a -> f a -> f a
append2 x m = x `mappend` x `mappend` m

twice :: (Foldable t, Applicative t, Monoid (t a)) => t a -> t a
twice = foldr (\l r -> append2 (pure l) r) mempty

twice' :: (Foldable t, Applicative t, Monoid (t a)) => t a -> t a
twice' = foldr (append2 . pure) mempty

data Expr
    = Num Double
    | Add Expr Expr
    | Var Char
    deriving (Show)

type Env = [(Char, Double)]
getvar :: Env -> Char -> Maybe Double
getvar = flip lookup

replaceDo :: Env -> Expr -> Maybe Expr
replaceDo env (Var c) = do
    c' <- getvar env c
    return (Num c')
replaceDo _ (Num n) = return (Num n)
replaceDo env (Add a b) = do
    a' <- replaceDo env a
    b' <- replaceDo env b
    return (Add a' b')

replaceBd :: Env -> Expr -> Maybe Expr
replaceBd env (Var c) = getvar env c >>= \a -> return (Num a)
replaceBd _ (Num n) = return (Num n)
replaceBd env (Add a b) =
    replaceBd env a
        >>= \a' ->
            replaceBd env b
                >>= \b' -> return (Add a' b')

replaceAp :: Env -> Expr -> Maybe Expr
replaceAp env (Var c) = Num <$> getvar env c
replaceAp _ (Num n) = return (Num n)
replaceAp env (Add a b) = Add <$> replaceAp env a <*> replaceAp env b