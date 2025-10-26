-- ./HiOrd/MultiplRecurssive.hs
module HiOrd.MultiplRecurssive where

-- Example 1: using recurssive
sumOfSquareRootsR :: (Ord a, Floating a) => [a] -> a
sumOfSquareRootsR [] = 0
sumOfSquareRootsR (x:xs)
  | x > 0     = sqrt x + sumOfSquareRootsR xs
  | otherwise = sumOfSquareRootsR xs

-- Example 2: using fmap / filter / reduction
sumOfSquareRootsF :: (Ord a, Floating a) => [a] -> a
sumOfSquareRootsF = foldl (+) 0 . fmap sqrt . filter (> 0)
-- sumOfSquareRootsF xs = foldl (+) 0 (sqrt <$> filter (> 0) xs)
sumOfSquareRootsF' :: (Ord a, Floating a) => [a] -> a
sumOfSquareRootsF' = foldl (\acc val -> sqrt val + acc) 0 . filter (> 0)

-- Example 3: using composition
sumOfSquareRootsC :: (Ord a, Floating a) => [a] -> a
sumOfSquareRootsC xs =
  foldl (.) id ((+) <$> sqrt <$> filter (>0) xs) $ 0
  -- foldl (.) id ((\x y -> sqrt x + y) <$> filter (>0) xs) $ 0

sumOfSquareRootsC' :: (Ord a, Floating a) => [a] -> a
sumOfSquareRootsC' xs = foldl (.) id ((\x y -> if x > 0 then sqrt x+y else y) <$> xs) $ 0

-- Samples

sample1, sample2 :: [Float]
sample1 = [1..10]
sample2 = [2,4..10]