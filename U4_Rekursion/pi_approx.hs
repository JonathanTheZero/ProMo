{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

pi_approx :: Int -> Double
pi_approx n = sqrt (sum [1 / fromIntegral (x ^ 2) | x <- [1 .. n]] * 6)

pi_approx' :: Int -> Double
pi_approx' 1 = 1
pi_approx' n = sqrt (_pi n * 6)
  where
    _pi 1 = 1
    _pi n = 1 / fromIntegral (n ^ 2) + _pi (n - 1)