-- ./HiOrd.MappingZipping.hs
module HiOrd.MappingZipping where

import Data.Char (toUpper)

-- Example 1: using recurssion
allSquaresR :: Num a => [a] -> [a]
allSquaresR []       = []
allSquaresR (x : xs) = x * x : allSquaresR xs

allToUpperR :: String -> String
allToUpperR []                 = []
allToUpperR (chr : restString) = toUpper chr : allToUpperR restString

data Point = Point Float Float 
distance :: Point -> Point -> Float
distance (Point x1 y1) (Point x2 y2) = (x1 - y1) * (x2 - y2)

distancesFromPointR :: Point -> [Point] -> [Float]
distancesFromPointR point [] = []
distancesFromPointR point (p : ps) = 
    distance point p : distancesFromPointR point ps

-- Example 2: using functor (fmap / map)
allSquaresF :: Num a => [a] -> [a]
allSquaresF = fmap square
    where
        square :: Num a => a -> a
        square x = x * x

allToUpperF :: String -> String
allToUpperF = fmap toUpper

distanceFromPointF :: Point -> [Point] -> [Float]
distanceFromPointF pB = fmap (distance pB)

-- Example 3: using zip / applicative functor
average :: Float -> Float -> Float
average x y = (x + y) / 2

zipp1 :: [Float]
zipp1 = zipWith average [1,2,3] [4,5,6]  

infixr 5 :-
data ZipList' a = Nothing' | a :- (ZipList' a) deriving (Show)

instance Functor ZipList' where
    fmap :: (a -> b) -> ZipList' a -> ZipList' b
    fmap f Nothing' = Nothing'
    fmap f (x:-xs) = f x :- fmap f xs

instance Applicative ZipList' where
    pure :: a -> ZipList' a
    pure x = x :- Nothing'
    (<*>) :: ZipList' (a -> b) -> ZipList' a -> ZipList' b
    (<*>) Nothing' _ = Nothing'
    (<*>) _ Nothing' = Nothing'
    (<*>) (h :- hs) (x :- xs) = h x :- (hs <*> xs)

ztol :: ZipList' a -> [a]
ztol Nothing' = []
ztol (x:-xs) = x:ztol xs

ltoz :: [a] -> ZipList' a
ltoz = foldr (:-) Nothing' 

zipp2 :: [Float]
zipp2 = ztol (average <$> ltoz [1,2,3] <*> ltoz [4,5,6])

-- instance Monad ZipList' where
--     (>>=) :: ZipList' a -> (a -> ZipList' b) -> ZipList' b
--     (>>=) Nothing' _ = Nothing'
--     (>>=) (x:-xs) f = let y:-_ = f x in y :- (xs >>= f)
--         -- where y:-_ = f x

--     -- (>>=) (x:-xs) f = y :- (xs >>= f)
--         -- where y:-_ = f x 

-- zipp3 :: [Float]
-- zipp3 = ztol $ do
--     x <- ltoz [1,2,3]
--     y <- ltoz [4,5,6]
--     return $ average x y
