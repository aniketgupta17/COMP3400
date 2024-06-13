module FirstCommon (firstCommon) where


import           Prelude hiding (filter, fmap, foldl, foldr, liftA2, map, mapM,
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

--}



--                                       ** THE TASK **

-- Implement a function which will find the most common element in a list. 
-- If there are several elements with this property, return the one that is encountered earlier. 
-- We guarantee that the list will contain at least one element.
--
--  Examples:
--
-- fistCommon [1, 2, 3, 1] == 1
-- firstCommon [1] == 1
-- firstCommon [2, 3, 1] == 2
--
-- This task is worth 10 POINTS.


-- Count occurrences of an element in a list
countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences _ [] = 0
countOccurrences x (y:ys)
  | x == y = 1 + countOccurrences x ys
  | otherwise = countOccurrences x ys

-- Find the most common element in a list
mostCommon :: Eq a => [a] -> a
mostCommon [] = error "Empty list"
mostCommon (x:xs) = mostCommon' x xs (countOccurrences x xs)
  where
    mostCommon' acc [] _ = acc
    mostCommon' acc (y:ys) count
      | countOccurrences y ys > count = mostCommon' y ys (countOccurrences y ys)
      | otherwise = mostCommon' acc ys count

-- Implement firstCommon
firstCommon :: Eq a => [a] -> a
firstCommon xs = mostCommon xs
