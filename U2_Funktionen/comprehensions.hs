ungerade :: Integral a => a -> Bool
ungerade x = x `mod` 2 /= 0

gerade :: Integral a => a -> Bool
gerade x = x `mod` 2 == 0

alleUnterschiedlich :: Eq a => a -> a -> a -> Bool
alleUnterschiedlich a b c = a /= b && b /= c && c /=a

alleUngeradenElemente :: Integral a => [a] -> [a]
alleUngeradenElemente list = [x | x <- list, ungerade x]

geradeZahlen2 list = [x | x <-list]