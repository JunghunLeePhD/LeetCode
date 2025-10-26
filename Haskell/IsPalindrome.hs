main :: IO()
main = do
    print $ filter isPalindrome [1..1000000]

isPalindrome :: Int -> Bool
isPalindrome x  
    | x < 0 = False
    | otherwise = reverse s == s
    where
        s :: String
        s = show x 


