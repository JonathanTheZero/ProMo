-- A1
f :: (a -> b -> c) -> (a -> b) -> a -> c
f g h x = g x (h x)

-- A2 Wurzel vervollständigen
magnitude :: Floating a => [a] -> a
magnitude = sqrt . sumOfSquares
  where
    sumOfSquares [] = 0
    sumOfSquares (x : xs) = x ^ 2 + sumOfSquares xs

magnitude' :: Floating a => [a] -> a
magnitude' = sqrt . sumOfSquares 0
  where
    sumOfSquares akk (x : xs) =                     -- In Musterlösung sumOfSquares akk xs
        if null xs
            then akk + x ^ 2
            else sumOfSquares ((x) ^ 2 + akk) xs    -- dadurch statt (x) (head xs)

magnitude'' :: Floating a => [a] -> a
magnitude'' = sqrt . foldr (\a r -> r + a * a) 0

-- A4 Binärbaume
data BB a = L | K (BB a) a (BB a)
w :: BB String
w = K (K (K L "(*)" L) "<$>" (K L "[3]" L)) "<*>" (K L "[2]" L) -- Baum zu prefix liste collecten

prefix :: BB a -> [a]
prefix L = []
prefix (K l w r) = w : prefix l ++ prefix r

infixC :: BB a -> [a]
infixC L = []
infixC (K l w r) = infixC l ++ [w] ++ infixC r

l :: [String]
l = ["(*)", "<$>", "[3]", "<*>", "[2]"] -- infixC w
_solution :: [Integer]
_solution = (*) <$> [3] <*> [2] -- = [6]

-- A5
fmapBB :: (a -> b) -> BB a -> BB b
fmapBB f L = L
fmapBB f (K l a r) = K (fmapBB f l) (f a) (fmapBB f r) -- fmap vervollständigen

instance Functor BB where
    fmap :: (a -> b) -> BB a -> BB b
    fmap = fmapBB -- BB als Functor implementieren

konvertiere :: Functor t => t Int -> t String
konvertiere = fmap show

showBB :: BB String
showBB = let b = K (K L 2 L) 1 (K (K L 4 L) 3 L) in konvertiere b -- show Functor implementieren

-- A6
data Polynom c e = Poly c e (Polynom c e) | PolySentinel deriving (Eq) -- Datentyp definieren

tuplesToPoly :: [(c, e)] -> Polynom c e
tuplesToPoly [] = PolySentinel
tuplesToPoly ((c, e) : xs) = Poly c e (tuplesToPoly xs) -- konvertierungsfunktion implementieren

showPoly :: (Show c, Show e, Show c, Show e) => Char -> Polynom c e -> String
showPoly v PolySentinel = ""
showPoly v (Poly c e PolySentinel) = concat [show c, "*", [v], "^", show e]
showPoly v (Poly c e rest) = concat [show c, "*", [v], "^", show e, " + ", showPoly v rest] -- Stringdarstellung

-- A7
reverse' :: (Foldable t, Applicative t, Monoid (t a)) => t a -> t a
reverse' = foldr (\l r -> r `mappend` pure l) mempty -- mit foldr und lambda

reverse'' :: (Foldable t, Applicative t, Monoid (t a)) => t a -> t a
reverse'' = foldr (flip mappend . pure) mempty -- ohne explizite Parameter/Lambdas

revTest :: (Foldable t, Applicative t, Monoid (t a)) => t a -> t a
revTest = foldl (\r l -> pure l `mappend` r) mempty -- auch mit foldl?

-- A8
data Expr
    = Num Double
    | Add Expr Expr
    | Var Char
    | Sqrt Expr
    deriving (Show)

type Env = [(Char, Double)]

evalDo :: Env -> Expr -> Maybe Double
evalDo _ (Num a) = return a
evalDo e (Add a b) = do
    a' <- evalDo e a
    b' <- evalDo e b
    Just (a' + b')
evalDo e (Var c) = getvar e c -- Funktion erweitern
evalDo e (Sqrt a) = do
    a' <- evalDo e a
    if a' < 0 then Nothing else Just (sqrt a')

getvar :: Env -> Char -> Maybe Double
--getvar e c = evalDo e (Var c)           --falsch, hat Parameter
getvar = flip lookup
