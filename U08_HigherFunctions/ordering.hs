{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

import Data.List (sort)

data Team = Team
    { name :: String
    , nW :: Int -- Anzahl der Siege
    , nD :: Int -- Anzahl der Unentschieden
    , nL :: Int -- Anzahl der Niederlagen
    , nGF :: Int -- Erzielte Tore
    , nGA :: Int -- Kassierte Tore
    }
    deriving (Eq, Show)

bayern :: Team
bayern = Team "Bayern Muenchen" 15 05 01 60 25
dortmund :: Team
dortmund = Team "Borussia Dortmund" 15 05 01 60 25
berlin :: Team
berlin = Team "Union Berlin" 15 05 01 35 30
leipzig :: Team
leipzig = Team "Corporation Leipzig" 10 05 06 50 30
wolfsburg :: Team
wolfsburg = Team "Wolfsburg" 10 05 06 50 30
schalke :: Team
schalke = Team "Schalke 04" 01 10 10 15 50

instance Ord Team where
    -- sorts descending
    compare :: Team -> Team -> Ordering
    compare a b
        | points a > points b = LT
        | points a < points b = GT
        | goalDifference a > goalDifference b = LT
        | goalDifference a < goalDifference b = GT
        | name a == "Bayern Muenchen" = LT
        | name a > name b = GT
        | name a < name b = LT
        | otherwise = EQ
    -- Monoid implementation
    {-compare t1 t2 =
        compare (points t1) (points t2)
            `mappend` compare (goalDifference t1) (goalDifference t2)
            `mappend` compare (bestTeam t1) (bestTeam t2)
            `mappend` compare (name t1) (name t2)
      where
        bestTeam t = name t == "Bayern Muenchen"-}

points :: Team -> Int
points t = 3 * nW t + nD t

goalDifference :: Team -> Int
goalDifference t = nGF t - nGA t

bundesliga :: [Team]
bundesliga = [bayern, berlin, schalke, dortmund, leipzig, wolfsburg]

sortedLeague :: [Team]
sortedLeague = sort bundesliga