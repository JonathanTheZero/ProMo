newtype RGB a = RGB (a, a, a) deriving (Show)

rgbVal :: (Ord a, Num a) => a -> a
rgbVal x | x > 255 = 255
rgbVal x | x < 0 = 0
rgbVal x = x

instance (Ord a, Num a) => Semigroup (RGB a) where
    (<>) :: Num a => RGB a -> RGB a -> RGB a
    (<>) (RGB (a1, b1, c1)) (RGB (a2, b2, c2)) = RGB (rgbVal (a1 + a2), rgbVal (b1 + b2), rgbVal (c1 + c2))

instance (Ord a, Num a) => Monoid (RGB a) where
    mempty :: (Ord a, Num a) => RGB a
    mempty = RGB (0, 0, 0)

red :: RGB Integer
red = RGB (255, 0, 0)
blue :: RGB Integer
blue = RGB (0, 255, 0)
stuff :: RGB Integer
stuff = RGB (100, 100, 100)

result :: RGB Integer
result = red `mappend` blue `mappend` stuff