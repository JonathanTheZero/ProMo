ungerade :: Integral a => a -> Bool
ungerade x = x `mod` 2 /= 0

gerade :: Integral a => a -> Bool
gerade x = x `mod` 2 == 0


alleUnterschiedlich :: Eq a => a -> a -> a -> Bool
alleUnterschiedlich a b c = a /= b && b /= c && c /= a

alleUngeradenElemente :: Integral a => [a] -> [a]
alleUngeradenElemente list = [x | x <- list, ungerade x]

geradeZahlen2 :: Integral a => [a] -> [a]
geradeZahlen2 list = [if gerade x then x else 2 * x | x <- list]

div7Rest5 :: Integer -> Integer -> [Integer]
div7Rest5 m n = [x | x <- [m .. n], x `mod` 7 == 5]

laenge :: Num a => [t] -> a
laenge list = sum [1 | _ <- list]

istBayern :: [String] -> [String]
istBayern list = [s | s <- list, sum [if st `elem` "bayern" then 1 else 0 | st <- s] > 0]

nurGrossBuchstaben :: [Char] -> [Char]
putStrLnNurGrossBuchstaben :: [Char] -> IO ()
-- nurGrossBuchstaben str = [l | l <- str, l < 'a' && l /= ' ']
nurGrossBuchstaben str = [l | l <- str, l `elem` ['A' .. 'Z']]
putStrLnNurGrossBuchstaben = putStrLn . nurGrossBuchstaben

faktoren :: Integer -> [Integer]
faktoren n = [x | x <- [1 .. n], n `mod` x == 0]

ggT :: Integer -> Integer -> Integer
ggT n m = maximum [x | x <- faktoren n, x `elem` faktoren m]

pytri :: (Num c, Eq c, Enum c) => c -> [(c, c, c)] -- :: Integer -> [(Integer, Integer, Integer)]
pytri n = [(a, b, c) | a <- [1 .. n], b <- [1 .. n], c <- [1 .. n], a ^ 2 + b ^ 2 == c ^ 2]