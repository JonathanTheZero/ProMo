-- Binary Search Tree
data BB a = L | K a (BB a) (BB a) deriving (Show)

-- [6, 8, 12, 1, 3, 9, 5]
inOrderedBST :: BB Integer
inOrderedBST = K 6 (K 1 L (K 3 L (K 5 L L))) (K 8 L (K 12 (K 9 L L) L))
balancedBST :: BB Integer
balancedBST = K 6 (K 3 (K 1 L L) (K 5 L L)) (K 9 (K 8 L L) (K 12 L L))

-- there HAS to be an easier solution
isBinarySearchTree :: Ord a => BB a -> Bool
isBinarySearchTree L = True
isBinarySearchTree (K w L L) = True
isBinarySearchTree (K w (K l ll lr) L)
    | w <= l = False
    | otherwise = isBinarySearchTree (K l ll lr)
isBinarySearchTree (K w L (K r rl rr))
    | r <= w = False
    | otherwise = isBinarySearchTree (K r rl rr)
isBinarySearchTree (K w (K l ll lr) (K r rl rr))
    | w <= l = False
    | r <= w = False
    | otherwise = isBinarySearchTree (K l ll lr) && isBinarySearchTree (K r rl rr)

depth :: (Num a, Ord a) => BB t -> a
depth L = 0
depth (K w l r) = 1 + max (depth l) (depth r)

insert :: Ord a => a -> BB a -> BB a
insert el L = K el L L
insert el (K w l r) | el == w = K w l r
insert el (K w l L) | el > w = K w l (K el L L)
insert el (K w L r) | el < w = K w (K el L L) r
insert el (K w l r)
    | el < w = K w (insert el l) r
    | el > w = K w l (insert el r)

-- Korrektur aus Tutorium
insert' :: Ord a => a -> BB a -> BB a
insert' e L = K e L L
insert' e (K w l r)
    | e > w = K w l (insert e r)
    | e < w = K w (insert e l) r
    | otherwise = K w l r

elem' :: Ord a => a -> BB a -> Bool
elem' el L = False
elem' el (K w l r)
    | el == w = True
    | el < w = elem' el l
    | el > w = elem' el r

leaves :: Num a => BB b -> a
leaves L = 0
leaves (K w L L) = 1
leaves (K w l r) = leaves l + leaves r