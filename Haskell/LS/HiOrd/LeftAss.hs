-- HiOrd/LeftAss.hs
module HiOrd.LeftAss where

import Data.Char (digitToInt)
-- Example 1: using recurssion
stringToIntR :: String -> Int
stringToIntR = stringToInvIntR . fastReverseR 
    where 
        stringToInvIntR :: String -> Int
        stringToInvIntR [] = 0
        stringToInvIntR (char:chars) = digitToInt char + 10 * stringToInvIntR chars

fastReverseR :: [a] -> [a]
fastReverseR [] = []
fastReverseR (x:xs) = fastReverseR xs ++ [x]

-- Example 2: using reduction
stringToIntF :: String -> Int
stringToIntF = foldl (\acc chr -> 10 * acc + digitToInt chr) 0

fastReverseF :: [a] -> [a]
fastReverseF = foldl (\accList x -> x : accList) []

-- Example 3: using reduction
stringToIntC :: String -> Int
stringToIntC = stringToInvIntC . fastReverseR 
    where 
        stringToInvIntC :: String -> Int
        stringToInvIntC chars = foldl (.) id ((\x y -> digitToInt x + 10*y) <$> chars) $ 0

stringToIntC' :: String -> Int
stringToIntC' chars = foldl (\f g -> g . f) id ((\x y -> digitToInt x + 10*y) <$> chars) $ 0

fastReverseC :: [a] -> [a]
fastReverseC xs = foldl (.) id ((\x y -> y ++ [x]) <$> xs) $ []

fastReverseC' :: [a] -> [a]
fastReverseC' xs = foldl (\f g -> g . f) id ((\x y -> x:y) <$> xs) $ []

-- Samples
sample1, sample2, sample3 :: String
sample1 = "1295123"
sample2 = "ABCDEFZ"
sample3 = "0123990"