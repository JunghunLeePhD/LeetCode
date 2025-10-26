-- ./HiOrd/Reductions.hs
module HiOrd.Reductions where 

-- Example 1: using recurrsion
minListR :: [Int] -> Int
minListR []     = maxBound
minListR (x:xs) = x `min` minListR xs

sumR :: Num a => [a] -> a
sumR []     = 0
sumR (x:xs) = x + sumR xs

allEvenR :: [Int] -> Bool
allEvenR []     = True
allEvenR (x:xs) = even x && allEvenR xs

concatR :: [[a]] -> [a]
concatR []       = []
concatR (xs:xss) = xs ++ concatR xss

reverseR :: [a] -> [a]
reverseR []     = []
reverseR (x:xs) = reverseR xs ++ [x]

-- Example 2: using reduction
minListF :: [Int] -> Int
minListF = foldl min maxBound

sumF :: Num a => [a] -> a
sumF = foldl (+) 0

allEvenF :: [Int] -> Bool
allEvenF = foldl (\acc val -> even val && acc ) True

concatF :: [[a]] -> [a]
concatF = foldl (++) []

reverseF :: [a] -> [a]
reverseF = foldl (\acc val -> val:acc) []

-- Example 3: using functional composition
minListC :: [Int] -> Int
minListC xs = foldl (.) id (min <$> xs) $ maxBound

sumC :: Num a => [a] -> a
sumC xs = foldl (.) id ((+) <$> xs) $ 0

allEvenC :: [Int] -> Bool
allEvenC xs = foldl (.) id ((\x y -> even x && y) <$> xs) $ True

concatC :: [[a]] -> [a]
concatC xs = foldl (.) id ((++) <$> xs) $ []

reverseC :: [a] -> [a]
reverseC xs = foldl (.) id ((\x y -> y++[x]) <$> xs) $ []

-- Application 1: fmap
fmapR :: (a -> b) -> [a] -> [b]
fmapR _ []  = []
fmapR f (x:xs)  = f x : fmapR f xs

fmapF :: (a -> b) -> [a] -> [b]
fmapF f = foldl (\acc val -> acc ++ [f val]) []

fmapC :: (a -> b) -> [a] -> [b]
fmapC f xs = foldl (.) id ((\x y -> f x:y) <$> xs) $ []

-- Application 2: filter
filterR :: (a -> Bool) -> [a] -> [a]
filterR _ [] = []
filterR f (x:xs) = if f x then x:filterR f xs else filterR f xs

filterF :: (a -> Bool) -> [a] -> [a]
filterF f = foldl (\acc val -> if f val then acc ++ [val] else acc) []

filterC :: (a -> Bool) -> [a] -> [a]
filterC f xs = foldl (.) id ((\x y -> if f x then x:y else y) <$> xs) $ []

-- Samples
samplef :: Int -> Int
samplef x = x * x

sample1 :: [Int]
sample1 = [1..10]

sample2 :: [Int]
sample2 = [2,4..10]

sample3 :: [[Int]]
sample3 = [sample1, sample2, [11,13..30]]