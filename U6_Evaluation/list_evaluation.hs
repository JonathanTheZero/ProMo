{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use const" #-}

f :: [c] -> c
f = head . tail
g :: p -> Integer
g = (\x -> 0)
h :: Integer -> Integer
h = (\x -> x * x * x)

x = g (h 0)
{-  g (h 0) in Applikativer Reihenfolge (Inside-Out)
    g (h 0)
    g ((\x -> x * x * x) 0)
    g (0 * 0 * 0)
    g 0
    (\x -> 0) 0
    0

    g (h 0) in normaler Reihenfolge (Outside-In)
    g (h 0)
    (\x -> 0) (h 0)
    0
-}

y = h (h 1)
{-  h (h 1) lazy Evaluation
    h = (\x -> x * x * x)                       [(h, (\x -> x * x * x))]
    h (h 1)                                     
        let a = h 1 in h a                      [(a, h 1), (h, (\x -> x * x * x))]
        h a
        (\x -> x * x * x) a
        a * a * a
            a
            h 1
            (\x -> x * x * x) 1
            1 * 1 * 1
            3
        3 * 3 * 3
        27
    27                                          [(h, (\x -> x * x * x))]
-}

z = f [1, 2, 3]
{-  f [1, 2, 3] lazy Evaulation
    f = head.tail                               [(f, head.tail)]
    f [1, 2, 3]
    let a = [1, 2, 3] in f a                    [(a, [1, 2, 3]), (f, head.tail)]
        f a
        head (tail a)
            let b = tail a in head b            [(b, tail a), (a, [1, 2, 3]), (f, head.tail)]
            head b
            b
                tail a
                tail [1, 2, 3]
                [2, 3]
            [2, 3]                              [(b, [2, 3]), (a, [1, 2, 3]), (f, head.tail)]
        head b
        head [2, 3]
        2
    2                                           [(f, head.tail)]
-}