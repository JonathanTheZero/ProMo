{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

data BB a = N a | K (BB a) a (BB a)

foldlBB :: (b -> a -> b) -> b -> BB a -> b
foldlBB f acc (N v) = f acc v
foldlBB f acc (K l v r) = foldlBB f (f (foldlBB f acc l) v) r

hasZero :: (Eq a, Num a) => BB a -> Bool
hasZero = foldlBB (\acc x -> (x == 0) || acc) False

containsZero :: Bool
containsZero = let bb = (K (K (N 1) 2 (N 3)) 4 (N 0)) in hasZero bb