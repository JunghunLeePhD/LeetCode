--  ./MapFilterReduce/MapFilterReduce.js
module MapFilterReduce.MapFilterReduce where

myjoin :: (Monoid m) => [m] -> m
myjoin [] = mempty
myjoin (x:xs) = x `mappend` myjoin xs

-- a natural transfomation from Maybe to List
zeta :: Maybe a -> [a]
zeta (Just x) = [x]
zeta Nothing = []

boolToMaybe :: (a -> Bool) -> (a -> Maybe a)
boolToMaybe p z =
    if p z
        then Just z
        else Nothing


-- Example 1: myfilter
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p = myjoin . fmap (zeta . pMaybe)
    where
        pMaybe = boolToMaybe p

myeven :: [Int] -> [Int]
myeven = myfilter even

myodd :: [Int] -> [Int]
myodd = myfilter odd


-- Example 2: myreduce
newtype End b = End (b -> b)
instance Semigroup (End b) where
    (<>) :: End b -> End b -> End b
    (<>) (End f) (End g) = End (g . f)
instance Monoid (End b) where
    mempty :: End b
    mempty = End id

myreduce :: (a -> End b) -> [a] -> End b
myreduce g = myjoin . fmap g

myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl f x ys =
    let
        g z = End (\w -> f w z)
        End h = myreduce g ys
    in
        h x

mysum :: (Num a) => [a] -> a
mysum = myfoldl (+) 0

myfactorial :: (Num a) => [a] -> a
myfactorial = myfoldl (*) 1


-- Example 3: fmap via reduce
-- myfmap :: (a -> b) -> [a] -> [b]
mymap :: (a -> b) -> [a] -> End [b]
mymap f = myjoin . fmap g 
    where
        -- g :: a -> End [b]
        g x = End (\y -> y `mappend` return (f x)) 

myfmap :: (a -> b) -> [a] -> [b]
myfmap f xs = h []
    where 
        End h = mymap f xs

mydouble :: [Integer] -> [Integer]
mydouble = myfmap (\z -> z*z)


-- Example 4: filter via reduce
myfilter' :: (a -> Bool) -> [a] -> End [a]
myfilter' p = myjoin . fmap g 
    where
        -- g :: a -> End [a]
        g x = End (\y -> y `mappend` zeta (boolToMaybe p x)) 

myfilter'' :: (a -> Bool) -> [a] -> [a]
myfilter'' p xs = pp []
    where
        End pp = myfilter' p xs

myeven'' :: [Int] -> [Int]
myeven'' = myfilter'' even

myodd'' :: [Int] -> [Int]
myodd'' = myfilter'' odd