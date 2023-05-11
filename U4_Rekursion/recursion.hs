-- normal recusrive
length' :: [a] -> Int
length' [] = 0
length' (x : xs) = 1 + length' xs

-- tail recursive
length'' :: [a] -> Int
length'' list = lengthAcc list 0
 where
  lengthAcc :: [a] -> Int -> Int
  lengthAcc [] acc = acc
  lengthAcc (_ : xs) acc = lengthAcc xs (acc + 1)

-- normal recursive
contains' :: (Eq a) => [a] -> a -> Bool
contains' (x : xs) elem = elem == x || contains' xs elem

-- tail recursive
contains'' :: (Eq a) => [a] -> a -> Bool
contains'' list elem = conAcc list elem False
 where
  conAcc :: (Eq a) => [a] -> a -> Bool -> Bool
  conAcc [] elem acc = acc
  conAcc (x : xs) elem acc = conAcc xs elem (acc || (x == elem))

-- normal recursive
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

-- tail recursive
reverse'' :: [a] -> [a]
reverse'' list = _rev list []
 where
  _rev :: [a] -> [a] -> [a]
  _rev [] acc = acc
  _rev (x : xs) acc = _rev xs (x : acc)

-- normal recursive
take' :: (Eq a) => Int -> [a] -> [a]
take' amount xs
  | amount >= length' xs = xs
  | amount <= 0 = []
take' amount (x : xs) = x : take' (amount - 1) xs

-- tail recursive
take'' :: (Eq a) => Int -> [a] -> [a]
take'' amount list = _take amount list []
 where
  _take :: Int -> [a] -> [a] -> [a]
  _take amount xs acc | amount <= 0 || null xs = acc
  _take amount (x : xs) acc = _take (amount - 1) xs (acc ++ [x])
