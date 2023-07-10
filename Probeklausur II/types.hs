{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}

reverse' :: (Foldable t, Applicative t, Monoid (t a)) => t a -> t a
reverse' ls = foldr (\l r -> r `mappend` (pure l)) mempty ls

reverse'' :: (Foldable t, Applicative t, Monoid (t a)) => t a -> t a
reverse'' = foldr (flip mappend . pure) mempty

reverseT :: (Foldable t, Applicative t, Monoid (t a)) => t a -> t a
reverseT = foldl (\l r -> pure r <> l) mempty
--reverseT = foldl (mappend . pure ) mempty