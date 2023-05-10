-- normal recusrive
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- tail recursive
length'' :: [a] -> Int
length'' list = lengthAcc list 0
  where
    lengthAcc :: [a] -> Int -> Int
    lengthAcc [] acc = acc
    lengthAcc (_:xs) acc = lengthAcc xs (acc + 1)

-- normal recursive
contains' :: (Eq a) => [a] -> a -> Bool
contains' (x:xs) elem = elem == x || contains' xs elem

--tail recursive
contains'' :: (Eq a) => [a] -> a -> Bool
contains'' list elem = conAcc list elem False
    where
        conAcc :: (Eq a) => [a] -> a -> Bool -> Bool
        conAcc [] elem acc = acc
        conAcc (x:xs) elem acc = conAcc xs elem (acc || (x == elem))

