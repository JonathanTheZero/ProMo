-- E = empty list, L = list with length >= 1
data ML a = E | L a (ML a) deriving (Show)
