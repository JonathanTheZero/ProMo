any' :: (a -> Bool) -> [a] -> Bool
any' func [] = False
any' func (x : xs)
    | func x = True
    | otherwise = any' func xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys