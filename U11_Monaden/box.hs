data Box a = Empty String | Full a deriving (Show)

instance Functor Box where
    fmap :: (a -> b) -> Box a -> Box b
    fmap f (Empty s) = Empty s
    fmap f (Full a) = Full (f a)

instance Applicative Box where
    pure :: a -> Box a
    pure = Full
    (<*>) :: Box (a -> b) -> Box a -> Box b
    (Empty s) <*> b2 = Empty s
    b1 <*> (Empty s) = Empty s
    (Full a) <*> b2 = fmap a b2

instance Monad Box where
    (>>=) :: Box a -> (a -> Box b) -> Box b
    (Empty s) >>= _ = Empty s
    (Full x) >>= f = f x
