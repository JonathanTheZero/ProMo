foo :: (a -> b -> c) -> (b -> a) -> b -> c
foo f1 f2 x1 = f1 (f2 x1) x1

--foo2:: (Double -> Double -> Double) -> Double -> Double

foo3:: [a] -> [a] -> [a]
foo3 a b = a ++ b
