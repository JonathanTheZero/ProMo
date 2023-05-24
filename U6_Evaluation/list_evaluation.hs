{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use const" #-}

f :: [c] -> c
f = head . tail
g :: p -> Integer
g = (\x -> 0)
h :: Integer -> Integer
h = (\x -> x * x * x)
