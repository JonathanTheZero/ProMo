import Debug.Trace

quicksort :: (Ord a, Show a) => [a] -> [a]
-- quicksort l | trace (show l) False = l -- visualizing
quicksort [] = []
quicksort xs | length xs == 1 = xs
quicksort (x:xs) = quicksort [y | y <- xs, y <= x] ++ [x] ++ quicksort [y | y <- xs, y > x]