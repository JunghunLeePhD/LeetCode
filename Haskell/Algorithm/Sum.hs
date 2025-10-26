module Algorithm.Sum where
import Control.Monad (guard)
import Algorithm.Data

twoSumIndex :: [Int] -> Int -> [Int]
twoSumIndex nums target =
    head . candidates nums $ target
    where
        candidates :: [Int] -> Int -> [[Int]]
        candidates nums target = do
            i <- [0..length nums-2]
            j <- [i+1..length nums-1]
            guard $ nums!! i + nums!! j == target
            return [i,j]

-- Disscusion. For third example, it returns only empty list.
-- classifyList :: (Ord a) => a -> a -> Writer [a] [a] -> Writer [a] [a]
-- classifyList stand x (Writer (ys, zs))
--     | stand <= x = Writer (ys, x:zs)
--     | stand > x = Writer (x:ys, zs)

-- decomposeListOnce :: (Ord a) => a -> ([a], Writer [a] [a]) -> ([a], Writer [a] [a])
-- decomposeListOnce stand ([], writer) = ([], writer)
-- decomposeListOnce stand (x:xs, writer) = (xs, classifyList stand x writer)

-- decomposeList :: (Ord a) => a -> Int -> ([a], Writer [a] [a]) -> ([a], Writer [a] [a])
-- decomposeList stand 0 = id
-- decomposeList stand n
--     | even n =
--         decomposeList stand (n `div` 2) . decomposeList stand (n `div` 2)
--     | otherwise =
--         decomposeList stand (n-1) . decomposeListOnce stand

-- splitListOnce :: (Ord a) => a -> [a] -> Writer [a] [a]
-- splitListOnce stand xs = snd . decomposeList stand (length xs) $ (xs, return [])

-- twoSum :: [Int] -> Int -> [[Int]]
-- twoSum nums target = do
--     x <- nonpositiveList
--     y <- positiveList
--     guard $ x + y == target 
--     return [x,y]
--     where
--         nonpositiveList, positiveList :: [Int]
--         (nonpositiveList, positiveList) =
--             runWriter $ splitListOnce (target `div` 2) nums

-- nums1, nums2, nums3 :: [Int]
-- nums1 = [2,7,11,15]
-- nums2 = [3,2,4]
-- nums3 = [3,3]
-- target1, target2, target3 :: Int
-- target1 = 9
-- target2 = 6
-- target3 = 6

-- -- Example 1:
-- -- Input: nums = [2,7,11,15], target = 9
-- -- Output: [0,1]
-- -- Explanation: Because nums[0] + nums[1] == 9, we return [0, 1].
-- -- Example 2:
-- -- Input: nums = [3,2,4], target = 6
-- -- Output: [1,2]
-- -- Example 3:
-- -- Input: nums = [3,3], target = 6
-- -- Output: [0,1]