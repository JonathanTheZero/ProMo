{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use even" #-}

a0 :: [Integer]
a0 = [1, 2, 3]

a1 :: [Char]
a1 = "ab" ++ ['c', 'd']

a2 :: [Double]
a2 = [1, 2.0, 3]

a3 :: Bool
a3 = (5, "Bier") == (1.1, [])

a4 :: [(String, Integer)]
a4 = [("a", 1), ("b", 2)]

-- Error: a5 = [1, 2, [3, 4, [5]]]

a6 :: [[[Integer]]]
a6 = [[[1], [2]], [], [[3], [4]]]

-- Error: a7 = () /= (9, 'z')

-- Functions:
f0 :: t -> Bool
f0 x = if f0 x == f0 x then f0 x else f0 x > f0 x

f1 :: Num t => t -> Bool
f1 x = if f1 x == f1 x then f1 x else (f1 (x + 1)) > (f1 (x + 2))

f2 :: Integral a => [a] -> [a]
f2 x = [y | y <- x, y `mod` 2 == 0]

f3 :: (Ord a, Enum a) => a -> a -> () -> ()
f3 x y z = if succ x < y then z else ()

f4 :: (Read a1, Show a2) => String -> a2 -> (a1, String)
f4 x y = (read x, show y)

f5 :: (Ord a, Num p) => a -> a -> p -> p
f5 x y z = if x > y then z else z + 1

f6 :: (Eq t, Num t) => t -> t
f6 0 = 1
f6 n = n * f6 (n - 1)