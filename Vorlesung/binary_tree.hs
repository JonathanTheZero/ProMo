import Control.Exception
import Data.Data (Typeable)

data BB a = L | K a (BB a) (BB a) deriving (Show)

b0 = L
b1 = K 1 L L
b2 = K 1 (K 2 L L) (K 3 L L)
b4 = K 1 (K 2 L L) (K 3 (K 4 L L) L)
b = K 'a' (K 'b' L L) (K 'c' (K 'd' L L) L)

includes :: Ord a => a -> BB a -> Bool
includes _ L = False
includes x (K w left _) | x < w = includes x left
includes x (K w _ _) | x == w = True
includes x (K w _ right) | x > w = includes x right

data BBExn = BBleer | BBkeinNachfolger deriving (Show, Typeable)
instance Exception BBExn

root :: BB a -> a
root L = throw BBleer
root (K x _ _) = x

left :: BB a -> BB a
left L = throw BBkeinNachfolger
left (K _ l _) = l

right :: BB a -> BB a
right L = throw BBkeinNachfolger
right (K _ _ r) = r

isEmpty :: BB a -> Bool
isEmpty L = True
isEmpty (K{}) = False

infixCollect :: BB a -> [a]
infixCollect L = []
infixCollect (K w l r) = infixCollect l ++ [w] ++ infixCollect r

prefixCollect :: BB a -> [a]
prefixCollect L = []
prefixCollect (K w l r) = w : prefixCollect l ++ prefixCollect r

postfixCollect :: BB a -> [a]
postfixCollect L = []
postfixCollect (K w l r) = postfixCollect l ++ postfixCollect r ++ [w]

infixCollAkk :: BB a -> [a]
infixCollAkk b = col b []
  where
    col L akk = akk
    col (K w l r) akk = col l (w : col r akk)

tiefendurchlauf :: b -> (a -> b -> b -> b) -> BB a -> b
tiefendurchlauf wL fK L = wL
tiefendurchlauf wL fK (K w l r) = fK w (tiefendurchlauf wL fK l) (tiefendurchlauf wL fK r)

_infix :: BB a -> [a]
_infix = tiefendurchlauf [] (\w l1 l2 -> l1 ++ [w] ++ l2)
_prefix :: BB a -> [a]
_prefix = tiefendurchlauf [] (\w l1 l2 -> [w] ++ l1 ++ l2)

breitendurchlauf :: BB a -> [a]
breitendurchlauf baum = hb [baum]
  where
    hb :: [BB a] -> [a]
    hb [] = []
    hb (L : list) = hb list
    hb (K w l r : list) = w : hb (list ++ [l, r])