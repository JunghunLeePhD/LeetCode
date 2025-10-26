module MPS23.Data where

class Domain a where
    measure :: a -> Double
    dimension :: a -> Int
    center :: a -> Double
    move :: Double -> a -> a
    scale :: Double -> a -> a

instance Domain Interval where
    measure :: Interval -> Double
    measure (start, end) = abs $ end - start
    dimension :: Interval -> Int
    dimension _ = 1
    center :: Interval -> Double
    center (start, end) = (start + end )/2
    move :: Double -> Interval -> Interval
    move factor (start, end) = (start + factor, end + factor)
    scale :: Double -> Interval -> Interval
    scale factor interval@(start, end) = (start, start + factor * measure interval)

type Interval = (Double, Double)
type RealFunction = Double -> Double
type Accuracy = Int