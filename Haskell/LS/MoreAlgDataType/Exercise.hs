module MoreAlgDataType.Exercise where

-- Exercise 1: The standard functions 
-- head :: [a] -> a and tail :: [a] -> [a] are partial. 
-- Implement total variants safeHead and safeTail 
-- by making use of Maybe in the function results.

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

-- Exercise 2: Write a function 
-- myLength :: [a] -> Int 
-- that, given a list l, returns the same result as length l. 
-- However, implement myLength without any explicit pattern matching on lists; 
-- instead, use the function safeTail from the previous exercise 
-- to determine whether you reached the end of the list 
-- and to get the list tail in case where the end has not been reached yet.
myLength :: [a] -> Int
myLength = foldl (\acc val -> 1+acc) 0 

-- Exercise 3: Implement a function 
-- deleteSorted :: Ord a => a -> [a] ->  [a] 
-- which removes a value passed as first argument 
-- from a sorted list given as the second argument. 
-- If the value does not occur in the list, the list is returned unchanged. 
-- Exploit the fact that the list is sorted: 
-- if an element is not present in the list, 
-- stop the search as early as possible.
deleteSorted :: Ord a => a -> [a] ->  [a] 
deleteSorted _ [] = []
deleteSorted x (y:ys)
    | x <= y    = x : y : ys
    | otherwise = y : deleteSorted x ys

