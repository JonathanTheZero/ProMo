{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use const" #-}

quadrat :: Integer -> Integer
quadrat = \x -> x * x
summe_quadrate :: Integer -> Integer -> Integer
summe_quadrate = \x y -> quadrat x + quadrat y
immer_null :: p -> Integer
immer_null = \x -> 0
f :: Integer -> Integer
f = \n -> if immer_null (quadrat n) /= n then summe_quadrate (n - 2) (n - 1) else n

{- Applikative Auswertung (inside out) von f 3
    f 3
    (\n -> if immer_null (quadrat n) /= n then summe_quadrate (n - 2) (n - 1) else n) 3
    if immer_null (quadrat 3) /= 3 then summe_quadrate (3 - 2) (3 - 1) else 3    
    if immer_null ((\x -> x * x) 3) /= 3 then (\x y -> quadrat x + quadrat y) 1 2 else 3
    if immer_null (3 * 3) /= 3 then quadrat 1 + quadrat 2 else 3
    if immer_null 9 /= 3 then (\x -> x * x) 1 + (\x -> x * x) 2 else 3
    if immer_null 9 /= 3 then 1 * 1 + 2 * 2 else 3
    if immer_null 9 /= 3 then 5 else 3
    if (\x -> 0) 9 /= 3 then 5 else 3
    if 0 /= 3 then 5 else 3
    5

    Vorteil normaler Auswertung: immer_null (quadrat n) wird sofort zu 0, dadurch wird insgesamt deutlich weniger in der Bedingung ausgewertet
-}