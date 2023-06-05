magnitude :: Floating a => [a] -> a
magnitude = sqrt . sumOfSquares
  where
    sumOfSquares [] = 0
    sumOfSquares (x : xs) = x ^ 2 + sumOfSquares xs

magnitude' :: Floating a => [a] -> a
magnitude' = sqrt . sumOfSquares 0
  where
    sumOfSquares akk (x : xs) =
        if null xs
            then akk + x ^ 2
            else sumOfSquares ((x) ^ 2 + akk) xs

magnitude'' :: Floating a => [a] -> a
magnitude'' = sqrt . foldr (\a r -> r + a ^ 2) 0

list :: Floating a => [a]
list = [3, 4, 8, -12]