module Main where

import Control.Monad ((>=>)) -- Import the Kleisli composition operator
import Data.Functor.Identity ( Identity(..), runIdentity )


f :: Double -> Double
f x = 4.1 * x * (1 - x)

-- main :: IO ()
-- main = print $ f 1

-- main :: IO ()
-- main = print $ f <$> [1..10]

-- main :: IO ()
-- main = print $ f . f . f . f <$> [(fromIntegral x) / 10 |  x <- [1..10]]

-- kPower :: Int -> (Double -> Double) -> (Double -> Double)
-- kPower n f
--     | n <= 0 = \x -> x
--     | n == 1 = f
--     | otherwise = kPower (n-1) f . f -- f^n = f^{n - 1} . f

-- main :: IO ()
-- main = print $ kPower 4 f <$> [fromIntegral x / 10 |  x <- [1..10]]

-- kPower :: Int -> (Double -> Double) -> (Double -> Double)
-- kPower n f
--     | n <= 0 = \x -> x
--     | even n = 
--         let
--             half = n `div` 2
--         in 
--             kPower half f . kPower half f -- f^{n} = f^{n / 2} . f^{n / 2}
--     | otherwise = kPower (n-1) f . f

-- main :: IO ()
-- main = print $ kPower 4 f <$> [fromIntegral x / 10 |  x <- [1..10]]


maybef :: (Double -> Double) -> Double -> Maybe Double
maybef f x
    | x < 0 = Nothing
    | x > 1 = Nothing
    | otherwise = Just $ f x

-- main :: IO()
-- main = print $ mf <$> [1..10]
--     where
--         mf :: Double -> Maybe Double
--         mf = maybef f

-- main :: IO()
-- main = print $ mf <$> [fromIntegral x / 10 | x <- [1..10]]
--     where
--         mf :: Double -> Maybe Double
--         mf = maybef f

-- main :: IO()
-- main = print $ (mf >=> mf >=> mf >=> mf) <$> [fromIntegral x / 10 | x <- [1..10]]
--     where
--         mf :: Double -> Maybe Double
--         mf = maybef f

kPower :: Monad m => Int -> (a -> m a) -> (a -> m a)
kPower n f
    | n == 0    = return 
    | even n    = let 
                    half = kPower (n `div` 2) f
                  in 
                    half >=> half -- f^n = f^(n/2) >=> f^(n/2)
    | otherwise = f >=> kPower (n - 1) f -- f^n = f >=> f^(n-1)

main :: IO()
main = print $ kPower 10 mf <$> [fromIntegral x / 10 | x <- [1..10]]
    where
        mf :: Double -> Maybe Double
        mf = maybef f

-- main :: IO ()
-- main = print $ kPower 100 (\x -> Identity (f x)) <$> [fromIntegral x / 10 |  x <- [1..10]]