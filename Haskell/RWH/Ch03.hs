-- ./RWH/Ch03.hs
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module RWH.Ch03 where


-- Exercise 1: Write the converse of fromList 
-- for the List type: a function that 
-- takes a List a and generates a [a].

data List a = Nil | Cons a (List a) deriving (Show)
-- fromList :: [a] -> List a
-- fromList xs = foldl (.) id (Cons <$> xs) Nil
-- fromList xs = foldl (.) id ((\z w -> Cons z w) <$> xs) Nil
-- fromList [] = Nil
-- fromList (x:xs) = Cons x (fromList xs)

newtype End b = End (b -> b)
instance Semigroup (End b) where
    (<>) :: End b -> End b -> End b
    (<>) (End f) (End g) = End (g . f)
instance Monoid (End b) where
    mempty :: End b
    mempty = End id

instance Semigroup (List a) where
    (<>) :: List a -> List a -> List a
    (<>) Nil Nil = Nil
    (<>) Nil ys = ys
    (<>) xs Nil = xs
    (<>) (Cons x xs) ys = Cons x (xs <> ys)
instance Monoid (List a) where
    mempty :: List a
    mempty = Nil
instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

class (Functor m) => MyFoldable m where
    myjoin :: m (End b) -> End b
    myreduce :: (a -> End b) -> (m a -> End b)
    myreduce g = myjoin . fmap g
    myfoldl :: (b -> a -> b) -> (b -> m a -> b)
    myfoldl f x ys = h x
        where
            End h = myreduce g ys
            g w = End (\z -> f z w)
instance MyFoldable List where
    myjoin :: List (End b) -> End b
    myjoin Nil = mempty
    myjoin (Cons x xs) =
        x `mappend` myjoin xs


toList :: List a -> [a]
toList xs = myfoldl (.) id ((:) <$> xs) []

instance MyFoldable [] where
    myjoin :: [End b] -> End b
    myjoin [] = mempty
    myjoin (x:xs) =
        x `mappend` myjoin xs


fromList :: [a] -> List a
fromList xs = myfoldl (.) id (Cons <$> xs) Nil


-- Exercise 2: Define a tree type that has only one constructor,
-- like our Java example. Instead of the Empty constructor,
-- use the Maybe type to refer to a node’s children.

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)
data Tree' a = Tree' (Maybe (a (Tree' a) (Tree' a)))

tree0, tree1, tree2 :: Tree Integer
tree0 = Leaf
tree1 = Node 1 Leaf Leaf
tree2 = Node 1 (Node 2 Leaf Leaf)  Leaf

tree0' = Tree' Nothing

-- p.69, Exercise 1: Write a function that computes the number of elements in a list.
-- To test it, ensure that it gives the same answers as the standard length function.

mylength1 :: [a] -> Int
mylength1 [] = 0
mylength1 (x:xs) = 1 + mylength1 xs
mylength2 :: [a] -> Int
mylength2 = foldl (\acc _ -> acc + 1) 0
mylength3 :: [a] -> Int
mylength3 xs = foldl (.) id ((+) <$> helper xs) 0
    where
        helper :: [a] -> [Int]
        helper xs = foldl (.) id ((\z w -> 1:w) <$> xs) []
mylength4 :: [a] -> Int
mylength4 xs = foldl (.) id ((+) <$> const 1 <$> xs) $ 0
mylength5 :: [a] -> Int
mylength5  xs = foldl (.) id ((+) <$> fmap (const 1) xs) $ 0
mylength6 :: [a] -> Int
mylength6 = sum . fmap (const 1)

--  p.69, Exercise 3: Write a function that computes the mean of a list, 
--  i.e., the sum of all elements in the list divided by its length. 
-- (You may need to use the fromIntegral function to convert the length 
-- of the list from an integer into a floating-point number.)

mymean1 :: [Integer] -> Float
mymean1 xs = mysum xs / mylength xs
    where
        mysum :: [Integer] -> Float
        mysum [] = 0
        mysum (x:xs) = fromIntegral x + mysum xs
        mylength :: [a] -> Float
        mylength = sum . fmap (const 1)

mymean2 :: [Integer] -> Float
mymean2 xs = mysum xs / mylength xs
    where
        mysum :: [Integer] -> Float
        mysum = sum . fmap fromInteger
        mylength :: [a] -> Float
        mylength = sum . fmap (const 1)

mymean3 :: [Integer] -> Float
mymean3 = mydivide (sum . fmap fromInteger) (sum . fmap (const 1))
    where
        mydivide :: (a -> Float) -> (a -> Float) -> a -> Float
        mydivide f g x = f x / g x

-- p.69, Exercise 4: Turn a list into a palindrome; 
-- i.e., it should read the same both backward and forward. 
-- For example, given the list [1,2,3], 
-- your function should return [1,2,3,3,2,1].

mypalidrome1 :: [a] -> [a]
mypalidrome1 [] = []
mypalidrome1 (x:xs) = [x] ++ mypalidrome1 xs ++ [x]

mypalidrome2 :: [a] -> [a]
mypalidrome2 xs = myidentity xs ++ myreverse xs
    where
        myidentity :: [a] -> [a]
        myidentity [] = []
        myidentity (x:xs) = [x] ++ myidentity xs
        myreverse :: [a] -> [a]
        myreverse [] = []
        myreverse (x:xs) = myreverse xs ++ [x]

mypalidrome3 :: [a] -> [a]
mypalidrome3 xs = myidentity xs ++ myreverse xs
    where
        myidentity :: [a] -> [a]
        myidentity = foldl (\acc val -> acc ++ [val]) []
        myreverse :: [a] -> [a]
        myreverse = foldl (\acc val -> [val] ++ acc) []

mypalidrome4 :: [a] -> [a]
mypalidrome4 xs = myidentity xs ++ myreverse xs
    where
        myidentity :: [a] -> [a]
        myidentity xs = foldl (.) id ((\z w -> [z]++w) <$> xs) []
        myreverse :: [a] -> [a]
        myreverse xs = foldl (.) id ((\z w -> w++[z]) <$> xs) []

mypalidrome5, mypalidrome5' :: [a] -> [a]
mypalidrome5 xs = foldl (.) id ((\z w -> [z]++w++[z]) <$> xs) []
mypalidrome5' = foldl (\acc val -> [val] ++ acc ++ [val]) [] -- [1,2,3] -> [3,2,1,1,2,3]

-- p.69, Exercise 5: Write a function that determines 
-- whether its input list is a palindrome.

isPalidrome1 :: (Eq a) => [a] -> Bool
isPalidrome1 xs =
    case length xs of
        even -> (mypalidrome1 . halfList $ xs) == xs
        odd -> False
        where
            halfList :: [a] -> [a]
            halfList = take (div (length xs) 2)

isPalidrome2 :: (Eq a) => [a] -> Bool
isPalidrome2 xs = (mypalidrome1 . halfList $ xs) == xs
    where
        halfList :: [a] -> [a]
        halfList = take (div (length xs) 2)

-- p.70, Exercise 6: Create a function that sorts a list of 
-- lists based on the length of each sublist. 
-- (You may want to look at the sortBy function from the Data.List module.)

mysortby1 :: [Int] -> [Int]
mysortby1 [] = []
mysortby1 xs = maxvalues xs ++ (mysortby1 . notmaxvalues $ xs)
    where
        mymax :: [Int] -> Int
        mymax xs = foldl (.) id (max <$> xs) minBound
        maxvalues :: [Int] -> [Int]
        maxvalues xs = filter (\z -> z == mymax xs) xs
        notmaxvalues :: [Int] -> [Int]
        notmaxvalues xs = filter (\z -> z /= mymax xs) xs

mysortby2 :: [Int] -> [Int]
mysortby2 = undefined
-- mysortby2 = foldl (.) id ((\z w -> maxvalues w ++ notmaxvalues w) <$> xs) []