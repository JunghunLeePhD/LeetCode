{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Chapter3.ArithmeticFunctions where
import Control.Monad (guard)
import Data.List ( intersect, sort )

isDivsor :: Int -> Int -> Bool
isDivsor n x = n `mod` x == 0

divisor :: Int -> [Int]
divisor n = filter (isDivsor n) [1..n]

gamma :: Int -> Int
gamma = length . divisor

isCoprime :: Int -> Int -> Bool
isCoprime n x = n `gcd` x == 1

coprimer :: Int -> [Int]
coprimer n = filter (isCoprime n) [1..n]

totient :: Int -> Int
totient = length . coprimer

delta :: Int -> Int
delta 1 = 1
delta _ = 0

one :: Int -> Int
one _ = 1

conv :: (Int -> Int) -> (Int -> Int) -> Int -> Int
conv f g n = sum (zipWith (*) (vfD n) (vgD n))
    where
        vfD :: Int -> [Int]
        vfD = fmap f . divisor
        vgD :: Int -> [Int]
        vgD = reverse . fmap g . divisor
        -- vhD :: (Int -> Int) -> Int -> [Int]
        -- vhD h = fmap h . divisor

gamma_conv :: Int -> Int
gamma_conv = conv one one

j :: Int -> Int -> Int
j k n = n^k

sigma :: Int -> Int -> Int
sigma k n = sum . fmap (^ k) $ divisor n

sigma_conv :: Int -> Int -> Int
sigma_conv k = conv one (j k)

isSquareFree :: Int -> Bool -- See Definition of conv
isSquareFree n = sum (zipWith (isCoprime) (vfD n) (vgD n))
    where
        vfD :: Int -> [Int]
        vfD = divisor
        -- vfD = fmap id . divisor
        vgD :: Int -> [Int]
        vgD = reverse . divisor
        -- vgD = reverse . fmap id . divisor
        sum :: [Bool] -> Bool
        sum = and
        -- sum = foldl (&&) True -- Recall sum == foldl (+) 0

squareFreezer :: Int -> [Int]
squareFreezer n = filter isSquareFree [1..n]

isPrime :: Int -> Bool
isPrime n = n > 1 && (null . factorizer $ n)
-- isPrime n = 2 == (length . divisor $ n)
-- isPrime n = (n-1) == (length . coprimer $ n)
    where
        factorizer :: Int -> [Int]
        factorizer n = filter (isFactor n) [2..(n-1)]
        isFactor :: Int -> Int -> Bool
        isFactor n x = n `mod` x == 0

primes :: Int -> [Int]
primes n = filter isPrime [1..n]

omega :: Int -> Int
omega = sum . fmap one . filter isPrime . divisor

mu :: Int -> Int
mu n
    | isSquareFree n = (-1)^omega n
    | otherwise = 0

isPrimePower :: Int -> Bool
isPrimePower n = n `elem` primePower n

isPrimeSquare :: Int -> Bool
isPrimeSquare n = n `elem` temp
    where temp = fmap (^2) . primes $ n

(***) :: [Int] -> [Int] -> [Int]
(***) = zipWith (*)

power :: [Int] -> Int -> [Int]
xs `power` n
    | n == 1 = xs
    | even n = (***) (xs `power` (n `div` 2)) (xs `power` (n `div` 2))
    | odd n = (***) (xs `power` (n-1))  xs
    | otherwise = []

power' :: [Int] -> Int -> [Int]
xs `power'` n
    | n == 1 = xs
    | even n = [1..(maximum xs)] `intersect` (***) (xs `power'` (n `div` 2)) (xs `power'` (n `div` 2))
    | odd n = [1..(maximum xs)] `intersect` (***) (xs `power'` (n-1))  xs
    | otherwise = []

primePower :: Int -> [Int]
primePower n = sort . concatMap (primes n `power`) $ [1..10]
-- primePower n = concatMap (`power'` [1..n]) primes n
-- primePower n = foldl (++) [] . fmap (primes n `power'`) $ [1..n]

-- nthPower :: Int -> [Int] -> [Int]
-- nthPower n = fmap (^n)

-- primeSquare :: Int -> Int -> [Int]
-- primeSquare m n =  nthPower m [2..n]

lambda :: Int -> Double
lambda n
    | isPrimePower n = log . fromIntegral $ n
    | otherwise = 0

