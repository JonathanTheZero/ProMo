{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}

f :: Num a => a -> a -> a
f x y = 2 * x + 3 * y
z :: Integer -> Integer -> Integer
z = flip f
y :: Integer -> Integer -> Integer
y = (\x -> f x)