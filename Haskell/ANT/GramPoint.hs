module MPS23.GramPoint where
import MPS23.Data ( RealFunction, Interval, Accuracy, Domain(center) )
import MPS23.RootFinding ( bisectMethodGlobal )

thetaFunction :: RealFunction
thetaFunction t
    | t <= 0 = 0 -- for well-defined function
    | otherwise = constTerm + mainTerm t + errorTerm t
    where
        constTerm :: Double
        constTerm = (-1) * pi/8
        mainTerm :: RealFunction
        mainTerm t = t/2 * log (t/(2*pi)) - t/2
        errorTerm :: RealFunction
        errorTerm t = 1/(48*t)

-- gramInterval :: Accuracy -> Interval -> [Interval]
-- gramInterval n = bisectMethodGlobal n $ sin . thetaFunction

-- gramGap :: Accuracy -> Interval -> [Double]
-- gramGap n interval = gap $ center <$> gramInterval n interval
--     where
--         gap :: [Double] -> [Double]
--         gap xs = zipWith (-) xs (0:xs)
