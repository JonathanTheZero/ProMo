op:: Int -> Int -> Int
op 0 0 = 1
op 0 1 = 0
op 1 0 = 0
op 1 1 = 1

-- Ist Monoid mit mempty = 1

instance Semigroup Int where
    (<>) :: Int -> Int -> Int
    (<>) = op

instance Monoid Int where
    mempty :: Int
    mempty = 1

x:: Int
x = 1
y:: Int
y = 0
z = mappend x y