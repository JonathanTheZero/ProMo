import Data.Char (toUpper)

length' :: [a] -> Integer
length' = foldr (\a b -> b + 1) 0

-- oops, wrong function
capitalizedInitials' :: String -> String
capitalizedInitials' str = unwords $ map capitalize (words str)
  where
    capitalize :: String -> String
    capitalize (x : xs) = toUpper x : xs

capitalizedInitials :: String -> String
capitalizedInitials xs = foldr (\a b -> toUpper (head a) : b) "" (words xs)

-- more efficient version
_capitalizedInitials :: String -> String
_capitalizedInitials = map (toUpper . head) . words

map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\acc b -> acc ++ [f b]) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\a list -> if f a then a : list else list) []

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f = foldr (\e res -> if not $ f e then [] else e : res) []