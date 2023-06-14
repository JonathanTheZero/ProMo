ungerade :: Integral a => a -> Bool
ungerade x = x `mod` 2 /= 0

gerade :: Integral a => a -> Bool
gerade x = x `mod` 2 == 0