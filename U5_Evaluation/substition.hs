{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}

quadrat :: Integer -> Integer
quadrat = \x -> x * x
a = let a = 2 in quadrat a

{- Substition:
    let a = 2 in quadrat a      []
    quadrat 2                   [(a, 2)]
    (\x -> x*x) 2               [(a, 2)]
    2 * 2                       [(a, 2)]
    4                           [(a, 4),(a, 2)]
-}
