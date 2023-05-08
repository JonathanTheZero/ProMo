main :: IO()
main = print (fib 30)

fib:: Integer -> Integer
fib n = fst (hfib n [(1, 1), (0,0)])

hfib :: Integer -> [(Integer, Integer)] -> (Integer, [(Integer, Integer)])
hfib n memo | n <= fst (head memo) = (myLookup n memo, memo)
hfib n memo = let (fibn1, memo1) = hfib (n-1) memo 
                  (fibn2, memo2) = hfib(n-2) memo1
                  fibn = fibn1 + fibn2
              in (fibn, (n, fibn) : memo2)

myLookup n ((m, fibn) : memo) = if n == m
    then fibn
    else myLookup n memo