-- ./HiOrd/Filtering.hs
module HiOrd.Filtering where

import Data.Char ( isDigit )

-- Example 1: using recurssion
extractDigitsR :: String -> String
extractDigitsR [] = []
extractDigitsR (chr : restString)
  | isDigit chr = chr : extractDigitsR restString
  | otherwise   =       extractDigitsR restString

newtype Point = Point Float deriving (Show)
distance :: Point -> Point -> Float
distance (Point x) (Point y) = abs $ x - y
inRadiusR :: Point -> Float -> [Point] -> [Point]
inRadiusR point radius []
  = []
inRadiusR point radius (p : ps)
  | distance point p <= radius = p : inRadiusR point radius ps
  | otherwise                  =     inRadiusR point radius ps

-- Example 2: using filter
extractDigitsF :: String -> String
extractDigitsF  = filter isDigit 


inRadiusF :: Point -> Float -> [Point] -> [Point]
inRadiusF point radius = filter (\z -> distance point z <= radius)

-- Example 3: using commutative diagram
-- ma * a ---> a
-- |  __com__/ | isDigit
-- | /   nat   |
-- ma -------> Bool

extractDigitsC :: [Char] -> [Char]
extractDigitsC = concat . fmap extractDigitC'
    where 
        -- extractDigitsN :: [Char] -> Bool
        -- extractDigitsN [] = False
        -- extractDigitsN _  = True
        extractDigitC' :: Char -> [Char]
        extractDigitC' x
            | isDigit x = [x]
            | otherwise = []

-- ma * a ---> a
-- |  __com__/ | \z -> distance point z <= radius 
-- | /   nat   |
-- ma -------> Bool

inRadiusC :: Point -> Float -> [Point] -> [Point]
inRadiusC pt rad = concat . fmap (inRadiusC' pt rad )
    where 
        inRadiusN :: [Point] -> Bool
        inRadiusN [] = False
        inRadiusN _  = True
        inRadiusC' :: Point -> Float -> Point -> [Point]
        inRadiusC' pt rad  x
            | distance pt x <= rad = [x]
            | otherwise = []