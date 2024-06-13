module PrincipalType (typeA, typeB, typeC, typeD, typeE) where

import           Prelude hiding (filter, fmap, foldl, foldr, liftA2, map, mapM,
                          mapM_, pure, replicate, return, reverse, sequence,
                          sequenceA, unzip, zip, zip3, zipWith, (*>), (<$),
                          (<$>), (<*), (<*>), (>>), (>>=))


-- Question 4: Easy

typeA (f, x) = f x

-- Question 5: Easy

typeB f x = (x, [[f (x, x)]])

-- Question 6: Medium


typeC f (x, y) (z, w) = f x y z w

-- Question 7: Medium

typeD f g x = f x (g x)

-- Question 8: Hard

typeE f g h = [g (h . f)]