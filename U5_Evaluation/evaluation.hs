{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use camelCase" #-}

quadrat :: Integer -> Integer
quadrat = \x -> x * x
summe_quadrate :: Integer -> Integer -> Integer
summe_quadrate = \x y -> quadrat x + quadrat y


{-  applikative Auswertungsreihenfolge/Inside-out
    

-}