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
replaceDo _ (Num n) = Just (Num n)
replaceDo env (Add a b) = do
    a' <- replaceDo env a
    b' <- replaceDo env b
    return (Add a' b')

hm :: Maybe Expr
hm = replaceDo [('x', 10), ('y', 3)] (Var 'x' `Add` (Num 2 `Add` Var 'y'))

isNothing :: Maybe Expr
isNothing = replaceDo [('x', 10)] (Var 'x' `Add` Var 'y')

replaceBd :: Env -> Expr -> Maybe Expr
replaceBd env (Var c) = getvar env c >>= \a -> return (Num a)
replaceBd _ (Num a) = return (Num a)
replaceBd env (Add a b) =
    replaceBd env a
        >>= \a' ->
            replaceBd env b
                >>= \b' -> return (Add a' b')

hm' :: Maybe Expr
hm' = replaceBd [('x', 10), ('y', 3)] (Var 'x' `Add` (Num 2 `Add` Var 'y'))

isNothing' :: Maybe Expr
isNothing' = replaceBd [('x', 10)] (Var 'x' `Add` Var 'y')

replaceAp :: Env -> Expr -> Maybe Expr
replaceAp env (Var c) = Num <$> getvar env c
replaceAp _ (Num n) = pure (Num n)
replaceAp env (Add a b) = Add <$> replaceAp env a <*> replaceAp env b

hm'' :: Maybe Expr
hm'' = replaceAp [('x', 10), ('y', 3)] (Var 'x' `Add` (Num 2 `Add` Var 'y'))

isNothing'' :: Maybe Expr
isNothing'' = replaceAp [('x', 10)] (Var 'x' `Add` Var 'y')
