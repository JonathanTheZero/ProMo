fib :: Integer -> Integer
fib n = hfib n 0 1
        where hfib 0 akk1 _ = akk1
              hfib n akk1 akk2 = hfib (n-1) akk2 (akk1+akk2)