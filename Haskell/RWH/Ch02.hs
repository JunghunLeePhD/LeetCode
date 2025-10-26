-- ./RWH/Ch02.hs
module RWH.Ch02 where

--- Example 1: last
-- myLast :: [a] -> a
-- myLast [] = error "No elements!"
-- myLast (x:[]) = x
-- myLast (_:xs) = myLast xs

myLast :: [a] -> a
myLast = myHead . myReverse

myLastF :: [a] -> a
myLastF = myHeadF . myReverseF

myLastFF :: [a] -> a
myLastFF xs = foldl (flip (.)) id (const <$> xs) $ error "No elements!"

myLastFFF :: [a] -> a
myLastFFF = foldl (\acc val -> val) $ error "No element!"

myHead :: [a] -> a
myHead [] = error "No elements!"
myHead (x:_) = x

myHeadF :: [a] -> a
myHeadF xs = foldl (.) id (const <$> xs) $ error "No elements!"

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myReverseF :: [a] -> [a]
myReverseF xs = foldl (.) id ((\z w -> w ++ [z]) <$> xs) $ []


--- Example 2: lastButOne
lastButOne :: [a] -> a
lastButOne [] = error "No elements!"
lastButOne (_:[]) = error "Only one elements!"
lastButOne (x:_:[]) = x
lastButOne (_:y:ys) = lastButOne $ y:ys
