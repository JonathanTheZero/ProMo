{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use camelCase" #-}

quadrat :: Integer -> Integer
quadrat = \x -> x * x
a = let a = 2 in quadrat a

{- Substition:
    let a = let a = 2 in quadrat a          [(a, let a = 2 in quadrat a)]
        let a = 2 in quadrat a              [(a, 2),(a, let a = 2 in quadrat a)]
            quadrat a                       [(a, 2),(a, let a = 2 in quadrat a)]
            quadrat 2                       [(a, 2),(a, let a = 2 in quadrat a)]
            (\x -> x * x) 2                 [(a, 2),(a, let a = 2 in quadrat a)]
            2 * 2                           [(a, 2),(a, let a = 2 in quadrat a)]
            4                               [(a, 2),(a, let a = 2 in quadrat a)]
        4                                   [(a, 4),(a, let a = 2 in quadrat a)]
    4                                       [(a, 4)]                   
-}  

summe_quadrate :: Integer -> Integer -> Integer
summe_quadrate = \x y -> quadrat x + quadrat y

x = summe_quadrate (5 - 2) (quadrat (3 - 1))
{- Applikative Auswertung (Inside-Out)
    summe_quadrate (5 - 2) (quadrat (3 - 1))
    summe_quadrate (5 - 2) (quadrat 2)
    summe_quadrate (5 - 2) ((\x -> x * x) 2)
    summe_quadrate (5 - 2) (2 * 2)
    summe_quadrate (5 - 2) 4
    summe_quadrate 3 4
    (\x y -> quadrat x + quadrat y) 3 4
    quadrat 3 + quadrat 4
    (\x -> x * x) 3 + (\x -> x * x) 4
    3 * 3 + 4 * 4
    9 + 16
    25
-}

{- Normale Auswertung (Outside-In)
    summe_quadrate (5 - 2) (quadrat (3 - 1))
    (\x y -> quadrat x + quadrat y) (5 - 2) (quadrat (3 - 1))
    quadrat (5 - 2) + quadrat (quadrat (3 - 1))
    (\x -> x * x) (5 - 2) + (\x -> x * x) (quadrat (3 - 1))
    (5 - 2) * (5 - 2) + (quadrat (3 - 1)) * (quadrat (3 - 1))
    3 * 3 + (((\x -> x * x)) (3 - 1) * ((\x -> x * x)) (3 - 1))
    9 + ((3 - 1) * (3 - 1)) * ((3 - 1) * (3 - 1))
    9 + (2 * 2) * (2 * 2)
    9 + 4 * 4
    9 + 16
    25
-}

{- Verzögerte Auswertung (Lazy evaluation) ??
    q = \x -> x * x                                         [(q, \x -> x * x)] (Funktionsname abgekürzt, da ich genauso lazy wie die evaluation bin)
    s_q = \x y -> q x + q y                                 [(s_q, \x y -> q x + q y) , (q, \x -> x * x)]
    s_q (5 - 2) (q (3 - 1))                                 [(s_q, \x y -> q x + q y) , (q, \x -> x * x)]
        let a = q (3 - 1) in s_q (5 - 2) a                  [(a, q (3 - 1)), (s_q, \x y -> q x + q y) , (q, \x -> x * x)]
        s_q (5 - 2) a
        (\x y -> q x + q y) (5 - 2) a
        q (5 - 2) + q a
            (\x -> x * x) (5 - 2) + (\x -> x * x) a
            (5 - 2) * (5 - 2) + a * a
            3 * 3 + q (3 - 1) * q (3 - 1)
            9 + (\x -> x * x) (3 - 1) * (\x -> x * x) (3 - 1)
            9 + (3 - 1) * (3 - 1) * (3 - 1) * (3 - 1)
            9 + 2 * 2 * 2 * 2
            9 + 16
            25
-}