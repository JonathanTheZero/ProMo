kopf :: String -> Char
kopf s = head s
-- kopf = head
-- kopf s = s !! 0

ende :: [Char] -> Char
ende s = last s
-- ende = last

rest :: [Char] -> [Char]
rest s = drop 1 s
--rest = drop 1

start :: String -> String
start s = take (length s - 1) s
