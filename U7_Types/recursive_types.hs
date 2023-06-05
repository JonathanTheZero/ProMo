-- E = empty list, L = list with length >= 1
data ML a = E | L a (ML a) deriving (Show)

list :: ML Int
list = L 1 (L 2 (L 3 (L 4 E)))

list2 :: ML Int
list2 = L 1 (L 2 (L 3 E))

myHead :: ML a -> a
myHead E = error "empty list"
myHead (L x _) = x

myAdd :: Num a => ML a -> ML a -> ML a
myAdd x E = E
myAdd E y = E
myAdd (L x xs) (L y ys) = L (x + y) (myAdd xs ys)

myAppend :: ML a -> ML a -> ML a
myAppend E E = E
myAppend E (L y ys) = L y (myAppend E ys)
myAppend (L x xs) y = L x (myAppend xs y)

toString :: Show a => ML a -> String
toString E = ""
toString (L x E) = show x
toString (L x xs) = show x ++ ", " ++ toString xs

myLess :: Ord a => ML a -> ML a -> Bool
myLess E E = False
myLess x E = False
myLess E x = True
myLess (L x xs) (L y ys) | x < y = True
myLess (L x xs) (L y ys) | x == y = myLess xs ys