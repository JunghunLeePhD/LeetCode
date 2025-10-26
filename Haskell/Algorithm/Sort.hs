module Algorithm.Sort where
import Algorithm.Data ( Writer(..) )
import Control.Monad ( (<=<) )

classifyMinimum :: (Ord a) => a -> Writer [a] [a] -> Writer [a] [a]
classifyMinimum x (Writer (ys,[])) = Writer (ys,[x])
classifyMinimum x (Writer (ys,z:zs))
    | x > z = Writer (x:ys,z:zs)
    | x == z = Writer (ys,x:z:zs)
    | x < z = Writer (ys ++ z:zs,[x])

-- Skew product??
decomposeMinimumOnce :: (Ord a) => ([a], Writer [a] [a]) -> ([a], Writer [a] [a])
decomposeMinimumOnce ([], writer) = ([], writer)
decomposeMinimumOnce (x:xs, writer) = (xs, classifyMinimum x writer)

-- State monad??
decomposeMinimum :: (Ord a) => Int -> ([a], Writer [a] [a]) -> ([a], Writer [a] [a])
decomposeMinimum 0 = id
decomposeMinimum n
    | even n =
        decomposeMinimum (n `div` 2) . decomposeMinimum (n `div` 2)
    | otherwise =
        decomposeMinimum (n-1) . decomposeMinimumOnce

-- length xs == complexity for taking minimum
splitMinimumOnce :: (Ord a) => [a] -> Writer [a] [a]
splitMinimumOnce xs = snd . decomposeMinimum (length xs) $ (xs, return [])

splitMinimum :: (Ord a) => Int -> [a] -> Writer [a] [a]
splitMinimum 0 = return
splitMinimum n
    | even n =
        splitMinimum (n `div` 2) <=< splitMinimum (n `div` 2)
    | otherwise =
        splitMinimum (n-1) <=< splitMinimumOnce

-- length xs == complexity for exchange order
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort xs = snd . runWriter $ splitMinimum (length xs) xs

-- quickSort :: (Ord a) => [a] -> [a]
-- quickSort [] = []
-- quickSort (x:xs) =
--     quickSort([z | z <- xs, z < x])
--     ++ [z | z <- xs, z == x]
--     ++ quickSort([z | z <- xs, z > x])


paritySort :: (Ord a, Integral a) => [a] -> [a]
paritySort xs = evens ++ odds
    where
        (evens, odds) = runWriter . snd . runWriter . decomposeEvenOrOdd $ xs

classifyEvenOrOdd :: (Ord a, Integral a) => [a] -> Writer (Writer [a] [a]) [a]
classifyEvenOrOdd [] = return []
classifyEvenOrOdd (x:xs)
    | even x = Writer (xs, Writer ([x],[]))
    | odd x = Writer (xs, Writer ([],[x]))
    where
        anonymous :: (Ord a, Integral a) => [a] -> Writer [a] [a]
        anonymous (x:xs)
            | even x = Writer ([x],[])
            | odd x = Writer ([],[x])

classifyEvenOrOddN :: (Ord a, Integral a) => Int -> [a] -> Writer (Writer [a] [a]) [a]
classifyEvenOrOddN n
    | n <= 0 = return
    | even n = classifyEvenOrOddN (n `div` 2) <=< classifyEvenOrOddN (n `div` 2)
    | odd n = classifyEvenOrOddN (n-1) <=< classifyEvenOrOdd

decomposeEvenOrOdd :: (Ord a, Integral a) => [a] -> Writer (Writer [a] [a]) [a]
decomposeEvenOrOdd xs = classifyEvenOrOddN (length xs) xs


shift :: [a] -> Writer [a] [a]
shift [] = return []
shift (x:xs) = Writer (xs,[x])

slice :: Int -> [a] -> [a]
slice n xs
    | n <= 0 = []
    | otherwise =
        snd . runWriter . shiftN n $ xs
    where
        shiftN :: Int -> [a] -> Writer [a] [a]
        shiftN n
            | n <= 0 = return
            | even n =
                shiftN (n `div` 2) <=< shiftN (n `div` 2)
            | odd n =
                shift <=< shiftN (n-1)

mergeSort :: (Ord a, Num a) => [a] -> Int -> [a] -> Int -> [a]
mergeSort xs m ys n = bubbleSort (slice m xs ++ slice n ys)