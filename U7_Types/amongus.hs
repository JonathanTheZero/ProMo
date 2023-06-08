data Role = Crewmate | Imposter deriving (Show, Eq)
data State = Alive | Dead deriving (Show, Eq)
data Color = Red | Blue | Purple | Yellow | Pink | Orange deriving (Enum, Show, Eq)

data Astronaut = Astronaut
    { username :: String
    , role :: Role
    , state :: State
    , tasks :: [String]
    , color :: Color
    }

instance Eq Astronaut where
    (==) :: Astronaut -> Astronaut -> Bool
    a == b = username a == username b && color a == color b

instance Show Astronaut where
    show :: Astronaut -> String
    show a = username a ++ ":" ++ show (color a)

isCrewmate :: Astronaut -> Bool
isCrewmate a = role a == Crewmate

isImpostor :: Astronaut -> Bool
isImpostor a = role a == Imposter

isAlive :: Astronaut -> Bool
isAlive a = state a == Alive

isDead :: Astronaut -> Bool
isDead a = state a == Dead

crewGewonnen :: [Astronaut] -> Bool
crewGewonnen xs = null ([x | x <- xs, isImpostor x, isAlive x]) && not (null ([x | x <- xs, isCrewmate x, isAlive x]))