{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
foo :: Integer -> Integer
foo = \x -> if x <= 1 then 1 else x * 10

y = foo 5

oo :: Integer -> Integer -> Integer
oo = \x y -> x * x + y * y
bar :: Integer -> Integer
bar = \z -> z * z