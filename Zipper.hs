module Zipper (Zipper (..), toZipper, zip1, unzip1, fullyZip, fullyUnzip) where

{--

*DO NOT import any modules*
*You are allowed to use anything available in Prelude and any syntax features* 

You are given the data type

    data Zipper a = Zipper [(a, a)] [a] [a]
    
which represents an actual zipper.
If you look at a real zipper, you'll see that it has some (possily none) pairs of teeth are connected below the slider,
and there are two (possibly empty) sequences of disconnected teeth above the slider.
                              
            "pairs of connected teeth below the slider"
                               v
    data Zipper a = Zipper [(a, a)] [a] [a]
                                     ^   ^
             "two sequences of disconnected teeth above the slider"

1. Your first task is make the Zipper a Functor:
    
    instance Functor Zipper where
        fmap = ...

fmap must abide all Functor laws in your implementation.

2. Implement a function to create a zipper from two lists. They can have different lengths of be empty. 
This function should return a completely "unzipped" Zipper.

    toZipper [] [1, 2, 3]        == Zipper [] [] [1, 2, 3]
    toZipper [4, 5, 6] [1, 2, 3] == Zipper [] [4, 5, 6] [1, 2, 3]
    toZipper [4, 5, 6] []        == Zipper [] [4, 5, 6] []
                                           ^^
            The newly-created Zipper is fully unzipped. There are no connected teeth.

3. Implement functions to move the slider in either direction.

        zip1 :: Zipper a -> Zipper a
    
    This function connects exactly one pair of teeth. 
    If either of the lists of disconnected teeth is empty, zip1 should have no effect on the Zipper.

        z = toZipper [1, 2] [4, 5, 6]

        zip1 z == Zipper [(1, 4)] [2] [5, 6]
        zip1 (zip1 z) == Zipper [(1, 4), (2, 5)] [] [6] -- All further applications of zip1 should have no effect
        zip1 (zip1 (zip1 z)) == Zipper [(1, 4), (2, 5)] [] [6]
    

        unzip1 :: Zipper a -> Zipper a

    This function is the opposite of zip1. If there is nothing to unzip, it should leave the Zipper as it is.

        z = Zipper [(1, 4), (2, 5)] [] [6]
        unzip1 z == Zipper [(1, 4)] [2] [5, 6]
        unzip1 (unzip1 z) == Zipper [] [1, 2] [4, 5, 6] -- All further applications of unzip1 should have no effect
        unzip1 (unzip1 (unzip1 z)) == Zipper [] [1, 2] [4, 5, 6] 

    
        fullyZip :: Zipper a -> Zipper a

    This function completely zips a Zipper to the point where at least one of the lists of disconnected teeth is empty.

        fullyZip (Zipper [(1, 2)] [3, 4, 5] [6, 7, 8, 9, 10]) == Zipper [(1, 2), (3, 6), (4, 7), (5, 8)] [] [9, 10]

        
        fullyUnzip :: Zipper a -> Zipper a

    This is the opposite of fullyZip.

        fullyUnzip (Zipper [(1, 2), (3, 6), (4, 7), (5, 8)] [] [9, 10]) == Zipper [] [1, 3, 4, 5] [2, 6, 7, 8, 9, 10]


Lists of disconnected teeth might be infinite, so plan your implementation accordingly.

This task is worth 10 POINTS.

--}

-- Define a data type for a zipper containing pairs and two lists
data Zipper a = Zipper [(a, a)] [a] [a]
    deriving (Show, Eq) -- Derive Show and Eq instances for easier debugging

-- Define the Functor instance for Zipper
instance Functor Zipper where
    -- Map a function over a Zipper, applying it to both the pairs and the lists
    fmap f (Zipper pairs left right) = Zipper (map (\(x, y) -> (f x, f y)) pairs) (map f left) (map f right)

-- Function to create a Zipper from two lists
toZipper :: [a] -> [a] -> Zipper a
toZipper left right = Zipper [] left right

-- Function to zip one element from each list together
zip1 :: Zipper a -> Zipper a
-- If there are elements in both lists, zip them together and add to the pairs
zip1 (Zipper pairs (l:ls) (r:rs)) = Zipper ((l, r) : pairs) ls rs
-- If either list is empty, return the original Zipper
zip1 zipper@(Zipper _ [] _) = zipper
zip1 zipper@(Zipper _ _ []) = zipper

-- Function to unzip one pair from the pairs and add its elements to the respective lists
unzip1 :: Zipper a -> Zipper a
-- If there are no pairs, return the original Zipper
unzip1 (Zipper pairs [] []) = Zipper pairs [] []
-- If there are pairs, unzip one pair and add its elements to the lists
unzip1 (Zipper ((l, r):pairs) ls rs) = Zipper pairs (l:ls) (r:rs)

-- Function to fully zip the lists together, pairing elements until one list is empty
fullyZip :: Zipper a -> Zipper a
-- Helper function to fully zip the lists
fullyZip (Zipper pairs left right) = fullyZip' (Zipper pairs left right)
  where
    -- Base case: If both lists are empty, return the original Zipper
    fullyZip' (Zipper pairs [] []) = Zipper pairs [] []
    -- Recursive case: Zip one element from each list and continue zipping
    fullyZip' (Zipper pairs (l:ls) (r:rs)) = fullyZip' (Zipper ((l, r) : pairs) ls rs)

-- Function to fully unzip the pairs, extracting elements until no pairs are left
fullyUnzip :: Zipper a -> Zipper a
-- If there are no pairs, return the original Zipper
fullyUnzip (Zipper [] ls rs) = Zipper [] ls rs
-- If there are pairs, fully unzip them by repeatedly extracting and adding elements to the lists
fullyUnzip (Zipper ((l, r):pairs) ls rs) = fullyUnzip' (Zipper pairs (l:ls) (r:rs))
  where
    -- Helper function to fully unzip the pairs
    fullyUnzip' (Zipper pairs left right) =
        let (left', right') = unzip pairs -- Unzip one pair
        in  fullyUnzip' (Zipper (drop 1 pairs) left' right') -- Recursively unzip the remaining pairs
