module Chapter3.Etc where
    above :: Double -> [Double] -> [Double]
    above x = filter (>= x)

    below :: Double -> [Double] -> [Double]
    below x = filter (<= x)

    isFactor :: Int -> Int -> Bool
    isFactor n x = n `mod` x == 0

    factorize :: Int -> [Int]
    factorize n = filter (isFactor n) [2..(n-1)]
    -- factorize xs = filter (isFactor n) [2..(n-1)]

    -- It is partial, make it full so 1 => False like
    -- isPrime k = (k > 1) && (null . factorize $ k)
    isPrime :: Int -> Bool
    isPrime = null . factorize

    primize :: [Int] -> [Int]
    primize = filter isPrime

    pi_legendre :: Double -> Int
    pi_legendre x = main x + error_1 x
        where
            main :: Double -> Int
            main = floor
            error_1 :: Double -> Int
            error_1 = floor

    comb :: [Int] -> [Int]
    comb xs = filter (\y -> y > last xs) [1..100]
