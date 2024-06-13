module Words (countWays) where

import Prelude hiding (filter, fmap, foldl, foldr, liftA2, map, mapM,
                      mapM_, pure, replicate, return, reverse, sequence,
                      sequenceA, unzip, zip, zip3, zipWith, (*>), (<$),
                      (<$>), (<*), (<*>), (>>), (>>=))

{-
You are not allowed to use
    - list comprehensions
    - do-notation
    - any imports
    - any zips or unzips
    - any maps and their operator counterparts (including, but not limited to fmap, amap, mapM, mapM_)
    - any functions on lists and their operator counterparts (including but not limited to sorts, folds, filters, reverse, replicate)
    - (<$>), (<*>), (*>). (>>=), (>>).

Gradescope will check your submission for any of the prohibited syntax, functions, operators and imports.

 ** Basically, the rule is: if you want to use something, implement it yourself! **

Execution time and memory consumption are limited.

Memory consumption is limited to 4 GB.

Execution time is limited to 10 minutes for the whole submission. Some tests may have their own time limits. If a single timed test exceeds the limit, only that test will fail. If the whole submission takes longer than 10 minutes to run, you will receive 0 for the whole submission.
-}

{-
                                       ** THE TASK **

A word w is written on the blackboard. Dr. H. A. Skell noticed that by removing some letters from w, he can obtain a new word v. Now he is interested in how many ways there are to obtain the word v from the original word by removing some (possibly none) letters.

Implement the function

    countWays :: String -> String -> Integer

which will do the calculations for him. A word is any sequence (possibly empty) of lower-case English letters.

    Examples:

countWays "haskell" "hall" == 1
countWays "energybill" "eel" == 2

This problem is worth 10 POINTS.
-}

-- Function to count the number of ways to obtain the target word from the original word
countWays' :: String -> String -> Int -> Int -> Int
countWays' _ [] _ _ = 1  -- If the target word is empty, there is only one way to obtain it
countWays' [] _ _ _ = 0  -- If the original word is empty and the target is not, it's not possible
countWays' (x:xs) (y:ys) i j
    | x == y    = countWays' xs ys (i+1) j + countWays' xs (y:ys) (i+1) (j+1)
    | otherwise = countWays' xs (y:ys) (i+1) j

-- Main function to calculate the number of ways
countWays :: String -> String -> Integer
countWays original target = fromIntegral $ countWays' original target 0 0