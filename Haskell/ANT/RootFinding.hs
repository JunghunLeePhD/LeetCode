module MPS23.RootFinding where
import Control.Monad ( guard, (<=<) )
import MPS23.Data
    ( RealFunction, Interval, Domain(measure, scale, move), Accuracy )

hasRoot :: RealFunction -> Interval -> Bool
hasRoot f (start, end)
    | f start * f end > 0 = False
    | f start * f end <= 0 = True

sectInterval :: Int -> Interval -> [Interval]
sectInterval section interval = do
    n <- [1..fromIntegral section]
    return $ move ((*(n-1)) . measure $ scaledInterval) scaledInterval
    where
        scaledInterval :: Interval
        scaledInterval = scale (1 / fromIntegral section) interval

bisectMethodLocal :: Accuracy -> RealFunction -> Interval -> [Interval]
bisectMethodLocal n f
    | n == 0 = return
    | even n = bisectMethodLocal (n `div` 2) f
    | otherwise = bisection f <=< bisectMethodLocal (n-1) f
    where
        bisection :: RealFunction -> Interval -> [Interval]
        bisection f interval = do
            bisectedInterval <- sectInterval 2 interval
            guard $ hasRoot f bisectedInterval
            return bisectedInterval

bisectMethodGlobal :: Accuracy -> RealFunction -> Interval -> [Double]
-- bisectMethodGlobal :: Accuracy -> RealFunction -> Interval -> [Interval]
bisectMethodGlobal n f interval =
    middlePoint <$> bisectInteval n f interval
    where
        -- sect the global interval into 1/100 local interval
        step :: Int
        step = (100*) . floor . measure $ interval
        bisectInteval :: Accuracy -> RealFunction -> Interval -> [Interval]
        bisectInteval n f = bisectMethodLocal n f <=< sectInterval step
        middlePoint :: Interval -> Double
        middlePoint (start, end) = (start + end)/2
