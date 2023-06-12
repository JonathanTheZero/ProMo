import Data.Char (toUpper)

length' :: [a] -> Integer
length' = foldr (\a b -> b + 1) 0

capitalizedInitials :: String -> String
capitalizedInitials str = unwords $ map capitalize (words str)
    where 
        capitalize:: String -> String
        capitalize (x:xs) = toUpper x : xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldl (\a b -> a ++ [f b]) []