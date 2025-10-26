-- File: RomanToInt.hs

numeralValue :: Char -> Int
numeralValue 'I' = 1
numeralValue 'V' = 5
numeralValue 'X' = 10
numeralValue 'L' = 50
numeralValue 'C' = 100
numeralValue 'D' = 500
numeralValue 'M' = 1000
numeralValue _   = 0

romanToInt :: String -> Int
romanToInt = process . fmap numeralValue
  where
    process :: [Int] -> Int
    process []       = 0  
    process [x]      = x  
    process (x:y:xs) 
      | x < y     = (y - x) + process xs      
      | otherwise = x + process (y:xs) 


main :: IO ()
main = do
    putStr "III -> "
    print . romanToInt $ "III"

    putStr "LVIII -> "
    print . romanToInt $ "LVIII"

    putStr "MCMXCIV -> "
    print . romanToInt $ "MCMXCIV"