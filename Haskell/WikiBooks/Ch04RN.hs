-- ./WikiBooks.Ch04RN.hs
module WikiBooks.Ch04RN where

import WikiBooks.Ch04
import System.Random
import Control.Monad (replicateM)

rollDie :: StdGen -> (Int, StdGen)
rollDie = randomR (1,6)

rollPair :: StdGen -> ((Int, Int), StdGen)
rollPair s0 =
    let
        (r1, s1) = rollDie s0
        (r2, s2) = rollDie s1
    in
        ((r1, r2), s2)

roll1 :: ((Int, Int), StdGen)
roll1 = rollPair (mkStdGen 666)

-- Exercises
rollSix :: StdGen -> ([Int], StdGen)
rollSix s0 =
    let
        (r1, s1) = rollDie s0
        (r2, s2) = rollDie s1
        (r3, s3) = rollDie s2
        (r4, s4) = rollDie s3
        (r5, s5) = rollDie s4
        (r6, s6) = rollDie s5
    in
        ([r1,r2,r3,r4,r5,r6], s6)

rollsix1 :: ([Int], StdGen)
rollsix1 = rollSix (mkStdGen 666)

rollN :: Int -> StdGen -> ([Int], StdGen)
rollN n s0
    | n <= 0 = error "Integer should be positive!"
    | n == 1 =
        let
            (r1, s1) = rollDie s0
        in
            ([r1], s1)
    | otherwise =
        let
            (r1, s1) = rollN (n-1) s0
            (r2, s2) = rollDie s1
        in
            (r1++[r2], s2)

rollN1 :: Int -> ([Int], StdGen)
rollN1 n = rollN n (mkStdGen 666)

luckyDouble :: StdGen -> (Int, StdGen)
luckyDouble s0 =
    let
        (a1,s1) = rollDie s0
    in
        case a1 of
            6 ->
                let
                    (a2, s2) = rollDie s1
                in
                    (a1+a2,s2)
            _ -> (a1,s1)

luckyDouble1 :: (Int, StdGen)
luckyDouble1 = luckyDouble (mkStdGen 666)

-- State
rollDieS :: MyState StdGen Int
rollDieS = fromS rollDie

rollDieS1 :: (Int, StdGen)
rollDieS1 = toS rollDieS (mkStdGen 666)

rollPairS :: MyState StdGen (Int, Int)
rollPairS = (,) <$> rollDieS <*> rollDieS
-- rollPairS = do
--     a1 <- rollDieS
--     a2 <- rollDieS
--     return (a1,a2)

rollPairS1 :: ((Int,Int), StdGen)
rollPairS1 = toS rollPairS (mkStdGen 666)

rollSixS :: MyState StdGen [Int]
rollSixS = do
    a1 <- rollDieS
    a2 <- rollDieS
    a3 <- rollDieS
    a4 <- rollDieS
    a5 <- rollDieS
    a6 <- rollDieS
    return [a1,a2,a3,a4,a5,a6]
-- rollSixS = sequence . replicate 6 $ rollDieS
-- rollSixS = replicateM 6 rollDieS

rollSixS1 :: ([Int], StdGen)
rollSixS1 = toS rollSixS (mkStdGen 666)

rollNS :: Int -> MyState StdGen [Int]
rollNS n = replicateM n rollDieS

rollNS1 :: Int -> ([Int], StdGen)
rollNS1 n = toS (rollNS n) (mkStdGen 666)

luckyDoubleS :: MyState StdGen Int
luckyDoubleS = do
    a1 <- rollDieS
    case a1 of
        6 -> do
            a2 <- rollDieS
            return $ a1 + a2
        _ -> return a1

luckyDoubleS1 :: (Int, StdGen)
luckyDoubleS1 = toS luckyDoubleS (mkStdGen 666)

rollDieDoubledS :: MyState StdGen Int
rollDieDoubledS = fmap (*2) rollDieS
-- rollDieDoubledS = do
--     r <- rollDieS
--     return (r * 2)

rollDieDoubledS1 :: (Int, StdGen)
rollDieDoubledS1 = toS rollDieDoubledS (mkStdGen 666)


rollTwoSummedS :: MyState StdGen Int
rollTwoSummedS = (+) <$> rollDieS <*> rollDieS
-- rollTwoSummedS = do
--     r1 <- rollDieS 
--     r2 <- rollDieS
--     return $ r1 + r2

rollTwoSummedS1 :: (Int, StdGen)
rollTwoSummedS1 = toS rollTwoSummedS (mkStdGen 666)

-- Exercise
happyDoubleS :: MyState StdGen Int
happyDoubleS = do
    r1 <- rollDieS
    r2 <- rollDieS
    case r1 of
        6 -> return $ 2 * (r1 + r2)
        _ -> return $ r1 + r2

happyDoubleS1 :: (Int, StdGen)
happyDoubleS1 = toS happyDoubleS (mkStdGen 666)

getRandomS :: Random a => MyState StdGen a
getRandomS = fromS random

allTypes :: MyState StdGen (Int, Float, Char, Integer, Double, Bool, Int)
allTypes = (,,,,,,) <$> getRandomS
                    <*> getRandomS
                    <*> getRandomS
                    <*> getRandomS
                    <*> getRandomS
                    <*> getRandomS
                    <*> getRandomS

allTypes1 = toS allTypes (mkStdGen 666)

randomEltS :: [a] -> MyState StdGen a
randomEltS [] = error "list must be nonempty!"
randomEltS xs = do
    a <- fromS $ randomR (1, length xs)
    return $ xs !! (a - 1)
-- randomEltS xs = do
--     s0 <- getS
--     let (value, _) = randomR (1,length xs) s0
--     return $ xs !! (value - 1)

-- randomEltS1 :: [a] -> a
randomEltS1 xs = toS (randomEltS xs) (mkStdGen 666)

getRandomPair :: IO (Int, Int)
getRandomPair = fmap (evalS rollPairS) newStdGen
-- getRandomPair = do
--     s <- newStdGen
--     return $ evalS rollPairS s

randomInputS :: MyState StdGen TurnstileInput
randomInputS = do
    -- b <- getRandomS
    b <- getRandomBoolS
    return $ if b then Coin else Push
    where
        getRandomBoolS :: MyState StdGen Bool
        getRandomBoolS = getRandomS
randomInputS1 = toS randomInputS (mkStdGen 666)



processingFst :: MyState a o -> MyState (a,b) o
processingFst m = do
    (s1,s2) <- getS
    let (o,s1') = toS m s1
    putS (s1',s2)
    return o

processingSnd :: MyState b o -> MyState (a,b) o
processingSnd m = do
    (s1,s2) <- getS
    let (o,s2') = toS m s2
    putS (s1,s2')
    return o

-- turnS :: TurnstileInput -> MyState TurnstileState TurnstileOutput
-- turnS = fromS . turn 
--     where
--         turn :: TurnstileInput -> TurnstileState -> (TurnstileOutput, TurnstileState)
--         turn Coin _        = (Thank, Unlocked)
--         turn Push Unlocked = (Open,  Locked)
--         turn Push Locked   = (Tut,   Locked)

data Lens cmb sub = Lens
    { view :: cmb -> sub,
    set  :: cmb -> sub -> cmb
    }

fstL :: Lens (a,b) a
fstL = Lens fst (\(_,y) x -> (x,y))
sndL :: Lens (a,b) b
sndL = Lens snd (\(x,_) y -> (x,y))