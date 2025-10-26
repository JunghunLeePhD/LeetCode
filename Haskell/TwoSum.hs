import Data.List (elemIndex)
import Data.Maybe (fromJust)

main:: IO()
main = case result of
    Just (a, b) -> putStrLn $ show a ++ "," ++ show b
    Nothing -> putStrLn "Nothing"
    where
        result :: Maybe (Int, Int)
        result = test [2,3,5,7,11] 9

test :: [Int] -> Int -> Maybe (Int, Int)
test nums target = (\a b -> (div len a, mod len b)) <$> temp <*> temp
-- test nums target = (,) <$> (mod len <$> temp) <*> (div len <$> temp)
-- test nums target = (mod len <$> temp, div len <$> temp)
    where
        step1 :: [Int]
        step1 = (+) <$> nums <*> nums
        temp :: Maybe Int
        temp = elemIndex target step1
        len :: Int
        len = length nums



