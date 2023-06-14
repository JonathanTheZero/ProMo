f :: Int -> Int
f 0 = 1
f 9 = 10
f n = n * f (n - 2)

s :: [Int] -> Int
s xs = ss xs 0
 where
  ss xs acc
    | null xs = acc
    | otherwise = ss xs (2 * head xs + acc)

-- Der Geltenungsbereich ist G = N0. Die Fälle 0 und 1 sind explizit als Rekursionsanker definiert.
-- Für ein beliebiges n ist Fib n als Fib (n-1) + Fib (n-2) definiert.
-- Da n um maximal 2 dekrementiert wird, terminiert die Funktion auf jeden Fall für Eingaben >= 0 - die Anker können bei der Subtraktion nicht "übersprungen" werden.
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

minimum' :: [Int] -> Int
minimum' (x : xs) = _min x xs
 where
  _min cmin [] = cmin
  _min cmin (x : xs)
    | x < cmin = _min x xs
    | otherwise = _min cmin xs
