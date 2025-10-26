module Algorithm.Monotone where

isMonotonic :: (Ord a, Num a) => [a] -> Bool
isMonotonic [] = True
isMonotonic [x] = True
isMonotonic (x:xs) =
    let 
        diff = zipWith (-) xs (x:xs)
    in
        maximum diff * minimum diff >= 0