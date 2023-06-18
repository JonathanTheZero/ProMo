-- Base functions for custom arithmetics
succ' :: Integer -> Integer
succ' x = x + 1
pred' :: Integer -> Integer
pred' x = x - 1
opp' :: Integer -> Integer
opp' x = -x

plus :: Integer -> Integer -> Integer
plus a b | b == 0 = a
plus a b = plus (succ' a) (pred' b)

minus :: Integer -> Integer -> Integer
minus a b | b == 0 = a
minus a b = minus (pred' a) (pred' b)

mult :: Integer -> Integer -> Integer
mult a b | b == 0 = 0
mult a b = _mult a (pred' b)
  where
    _mult x y | y == 0 = x
    _mult x y = _mult (plus x a) (pred' y)

fact :: Integer -> Integer
fact n | n <= 1 = 1
fact n = mult n (fact $ pred' n)