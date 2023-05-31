-- Binärbaum
class Eq a => BinBaum b a where
    istIn, istNichtIn :: a -> b a -> Bool
    istIn wert baum = not (istNichtIn wert baum)
    istNichtIn wert baum = not (istIn wert baum)

data BBKM a = L | MK a (BBKM a) (BBKM a) deriving (Show)

instance Eq a => BinBaum BBKM a where
    istIn x (MK w _ _) | x == w = True
    istIn x (MK _ links rechts) = istIn x links || istIn x rechts
    istIn _ _ = False