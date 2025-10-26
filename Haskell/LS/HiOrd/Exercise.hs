-- ./HiOrd/Exercise.hs
module HiOrd.Exercise where

-- Exercise 1: Define our old friend 
-- natSum :: Num a => a -> a 
-- (which sums the numbers from 1 up to the given argument) in terms of
-- enumFromTo n m
--   | n > m     = []
--   | otherwise = n : enumFromTo (n + 1) m
-- and one of the list combinators introduced in this chapter.

natSumR :: (Num a, Ord a) => a -> a
natSumR x
    | x < 0 = 0
    | otherwise = x + natSumR (x - 1)

natSumF :: (Enum a, Num a, Ord a) => a -> a
natSumF x = foldl (+) 0 [1..x]

natSumC :: (Enum a, Num a, Ord a) => a -> a
natSumC x = foldl (.) id ((+) <$> [1..x]) $ 0

-- enumFromToC :: (Enum a, Ord a, Num a) => a -> a -> [a]
-- enumFromToC n m = foldl (.) ((\x y -> x : [y]) <$> [n..m]) $ []
-- enumFromToF :: (Enum a, Ord a, Num a) => a -> a -> [a]
-- enumFromToF n m = foldl () []

-- Exercise 2: The map function is just a special case of foldr. 
-- Can you rewrite the map definition in terms of foldr? 
-- Complete the following definition:
-- map :: (a -> b) -> [a] -> [b]
-- map f = foldr …
mapR :: (a -> b) -> [a] -> [b]
mapR _ [] = []
mapR f (x:xs) = f x : mapR f xs

mapF :: (a -> b) -> [a] -> [b]
mapF f = foldl (\acc val -> acc ++ [f val]) []

mapC :: (a -> b) -> [a] -> [b]
mapC f xs = foldl (.) id ((\x y -> f x : y) <$> xs) $ []

-- Exercise 3: What does foldr (:) [] do if applied to a list?
whatF :: [a] -> [a]
whatF = foldr (:) []

whatR :: [a] -> [a]
whatR [] = []
whatR (x:xs) = x : whatR xs

whatC :: [a] -> [a]
whatC xs = foldl (.) id ((:) <$> xs) $ []

-- Exercise 4: Rewrite the spiralRays function from Spirals, Snowflakes & Trees: Recursion in Pictures
-- spiralRays :: Int -> Colour -> Line -> Picture
-- spiralRays n colour line@(p1, p2)
--   | n <= 0    = []
--   | otherwise = (colour, [p1, p2]) : spiralRays (n - 1) newColour newLine
--   where
--    newColour = fade colour
--    newLine   = scaleLine 1.02 (rotateLine (pi / 40) line)
-- to accept a function which calculates the current colour, instead of using the fade function. In other words, want it to have the following signature:
-- spiralRays :: Int -> (Int -> Colour) -> Line -> Picture


-- Exercise 5: Rewrite the function from the previous question, such that it uses list combinators as follows:
-- spiralRays :: Int -> (Int -> Colour) -> Line -> Picture
-- spiralRays n colourFun line = map spiralFun $ enumFromTo 1 n
--   where
--    spiralFun :: Int -> (Colour, Path)
--    spiralFun n = …
-- Otherwise, it should produce the same pictures as the previous version.


-- Samples

sample1, sample2, sample3 :: [Int]
sample1 = [1..10]
sample2 = [2,4..10]
sample3 = [1,3..10]