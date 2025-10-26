toDigit :: (Integral a) => a -> [a]
toDigit 0 = []
toDigit x = toDigit (x `div` 10) <> [x `mod` 10]

toDigitReverse :: (Integral a) => a -> [a]
toDigitReverse = reverse . toDigit

doubleEveryOther :: (Integral a) => [a] -> [a]
doubleEveryOther xs =
    let
        (y:ys) = reverse xs
    in
        reverse ((*2) <$> ys) <> [y]

sumDigits :: (Integral a) => [a] -> a
sumDigits [] = 0
sumDigits xs = sum $ xs >>= toDigit

validate :: Integer -> Bool
validate n 
    | (sumDigits . doubleEveryOther . toDigit $ n) `mod` 10 == 0 = True
    | otherwise = False

main :: IO()
main = do
    print $ toDigit test
    print $ doubleEveryOther . toDigit $ test
    print $ sumDigits . doubleEveryOther . toDigit $ test
    print $ validate test
    where
        test = 4012888888881884