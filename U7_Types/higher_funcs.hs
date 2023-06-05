any' :: (a -> Bool) -> [a] -> Bool
any' func [] = False
any' func (x : xs)
    | func x = True
    | otherwise = any' func xs