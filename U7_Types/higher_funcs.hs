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
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

unzipWith :: (t -> (a, b)) -> [t] -> ([a], [b])
unzipWith f [] = ([], [])
unzipWith f xs = (unzipA f xs, unzipB f xs)
  where
    unzipA :: (t -> (a, b)) -> [t] -> [a]
    unzipA f [] = []
    unzipA f (x : xs) = fst (f x) : unzipA f xs
    unzipB :: (t -> (a, b)) -> [t] -> [b]
    unzipB f [] = []
    unzipB f (x : xs) = snd (f x) : unzipB f xs

-- and now the more beautiful way, thanks ChatGPT
unzipWith' :: (t -> (a, b)) -> [t] -> ([a], [b])
unzipWith' f [] = ([], [])
unzipWith' f (x : xs) =
    let
        (a, b) = f x
        (as, bs) = unzipWith' f xs
     in
        (a : as, b : bs)