import Data.List (foldl')

main :: IO()
main = do
    print $ longestCommonPrefix <$> 
        [["flower", "flow", "flight"],
        ["interstate", "interstellar", "interior"],
        ["dog", "racecar", "car"],
        ["apple", "application", "apply"],
        ["flow", "flower", "flowing"],
        ["hello", "hello", "hello"],
        [],
        ["automation"],
        ["start", "", "starter"],
        ["", "begin", "below"],
        ["ca", "car", "carbon"],
        ["a", "a", "a", "a", "a", "a", "a"],
        ["Apple", "Application", "Apply"],
        ["Apple", "apple", "APPLY"],
        ["100", "1000", "10"],
        ["_internal", "_interrupt", "_interval"]]

longestCommonPrefix :: [String] -> String
longestCommonPrefix [] = mempty
longestCommonPrefix (x:xs) = foldl' getIntersection x xs

getIntersection :: (Eq a) => [a] -> [a] -> [a]
getIntersection _ [] = []
getIntersection [] _ = []
getIntersection (x:xs) (y:ys)
    | x == y = x : getIntersection xs ys
    | otherwise = []