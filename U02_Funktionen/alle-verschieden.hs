alleUnterschiedlich :: Eq a => a -> a -> a -> Bool
alleUnterschiedlich a b c = a /= b && b /= c && c /=a