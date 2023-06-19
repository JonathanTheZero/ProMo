data BB a = L | K a (BB a) (BB a) deriving (Show)

instance Functor BB where
    fmap :: (a -> b) -> BB a -> BB b
    fmap f L = L
    fmap f (K w l r) = K (f w) (fmap f l) (fmap f r)

