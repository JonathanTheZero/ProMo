data BB a = L | K (BB a) a (BB a)

pref :: BB t -> [t]
pref L = []
pref (K l w r) = w : pref l ++ pref r

inf :: BB a -> [a]
inf L = []
inf (K l w r) = inf l ++ [w] ++ inf r

w' :: BB String
w' = K (K (K L "1" L) "+" (K L "2" L)) "==" (K L "3" L)
w :: BB String
w = K (K (K L "1" L) "+" (K L "2" L)) "==" (K L "3" L)

w'' :: BB String
w'' = K
  (K
    (K L "1" L)
    "+"
    (K L "2" L)
  )
  "=="
  (K L "3" L)
