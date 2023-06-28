newtype ComplexNumber a = C (a, a)

instance Num a => Semigroup (ComplexNumber a) where
    (<>) :: Num a => ComplexNumber a -> ComplexNumber a -> ComplexNumber a
    (<>) (C (a1, b1)) (C (a2, b2)) = C (a1 + a2, b1 + b2)

instance Num a => Monoid (ComplexNumber a) where
    mempty :: Num a => ComplexNumber a
    mempty = C (0, 0)

instance (Show a, Num a, Ord a) => Show (ComplexNumber a) where
    show :: (Show a, Num a, Ord a) => ComplexNumber a -> String
    show (C (a1, b1)) = show a1 ++ (if b1 >= 0 then "+" else "") ++ show b1 ++ "i"

x :: ComplexNumber Integer
x = C (0, -3)
y :: ComplexNumber Integer
y = C (3, -2)
z :: ComplexNumber Integer
z = mappend x y