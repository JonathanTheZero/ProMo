import Debug.Trace

fak1 :: Integer -> Integer
fak1 0 = 1
fak1 n = n * fak1 (n - 1)

fak2 :: Integer -> Integer
fak2 x | trace ("call: fak" ++ show x) False = x
fak2 0 = 1
fak2 n = n * fak2 (n - 1)

fak3 :: Integer -> Maybe Integer
fak3 n | n < 0 = Nothing
fak3 n | n == 0 = Just 1
fak3 n | otherwise = let Just m = fak3 (n - 1) in Just (n * m)