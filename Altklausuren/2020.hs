{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

r :: (b -> c) -> (a -> b) -> a -> c
r = \f g x -> f (g x)

-- k :: Int -> Int
-- c:: a -> b -> a

produkt :: Integral a => a -> a
produkt n
    | n <= 1 = 1
    | otherwise = produkt (n - 1) * n

produkt' :: Integral a => a -> a
produkt' n = prod n 1
  where
    prod n akk = if n == 1 then akk else prod (n - 1) n * akk

produkt'' :: Integral a => a -> a
produkt'' = \n ->
    let prod = \n akk -> if n == 1 then akk else prod (n - 1) akk * n
     in prod n 1

qsum :: Double -> Double -> Double
qsum = \a b -> a * a + b * b
wurzel :: Double -> Double
wurzel = \n -> if n < 0 then 0 else sqrt n
x :: p -> Double
x = \n -> wurzel (qsum 3 4)
y :: p -> Double
y = \n -> wurzel (-4)

{- y (x 0) applikativ

y (x 0)
y ((\n -> wurzel (qsum 3 4)) 0)
y (wurzel (qsum 3 4))
y (wurzel ((\a b -> a*a + b*b) 3 4))
y (wurzel (3*3 + 4*4))
y (wurzel 25)
y ((\n -> if n<0 then 0 else sqrt n) 25)
y (sqrt 25)
y (5)
(\n -> wurzel (-4)) 5
wurzel (-4)
(\n -> if n<0 then 0 else sqrt n) (-4)
0

-}

{- y (x 0) normale Reihenfolge

y (x 0)
(\n -> wurzel (-4)) (x 0)
wurzel (-4)
(\n -> if n<0 then 0 else sqrt n) (-4)
0

-}

{- (x 0) lazy evaluation

x 0
(\n -> wurzel (qsum 3 4)) 0
-}

data BB a = L | K (BB a) a (BB a)

pref :: BB a -> [a]
pref L = []
pref (K l w r) = w : pref l ++ pref r

inf :: BB a -> [a]
inf L = []
inf (K l w r) = inf l ++ [w] ++ inf r

w :: BB String
w = K (K (K L "2" L) "*" (K L "3" L)) "/=" (K L "5" L)

foldrBB :: (a -> b -> b) -> b -> BB a -> b
foldrBB f b L = b
foldrBB f b (K l a r) = foldrBB f (f a (foldrBB f b r)) l

test :: String
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

reverse' :: (Foldable t, Applicative t, Monoid (t a)) => t a -> t a
reverse' = foldr (\l r -> r `mappend` pure l) mempty

reverse'' :: (Foldable t, Applicative t, Monoid (t a)) => t a -> t a
reverse'' = foldr (flip mappend . pure) mempty

data Expr
    = Num Double
    | Add Expr Expr
    | Sub Expr Expr
    deriving (Show)

eval :: Expr -> Maybe Double
eval (Num a) = if a < 0 then Nothing else Just a
eval (Add a b) = case eval a of
    Nothing -> Nothing
    Just a' -> case eval b of
        Nothing -> Nothing
        Just b' -> Just (a' + b')

evalDo :: Expr -> Maybe Double
evalDo (Num a) = if a < 0 then Nothing else return a
evalDo (Add a b) = do
    a' <- evalDo a
    b' <- evalDo b
    return (a' + b')

evalBd :: Expr -> Maybe Double
evalBd (Num a) = if a < 0 then Nothing else return a
evalBd (Add a b) = evalBd a >>= \a' -> evalBd b >>= \b' -> return (a' + b')