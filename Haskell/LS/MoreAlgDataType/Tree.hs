module MoreAlgDataType.Tree where

insert :: a -> [a] -> [a]
insert = (:)

isElement :: Eq a => a -> [a] -> Bool
isElement _ []     = False
isElement a (x:xs)
    | a == x         = True
    | otherwise      = isElement a xs

-- isElement' :: Eq a => a -> [a] -> Bool
-- isElement' z xs = foldl (.) id ((\x y -> if x == z then True else ) <$> xs) $ []

-- Section 1: Sorted lists
-- precondition: list is sorted in increasing order
isElementSorted :: Ord a  => a -> [a] -> Bool
isElementSorted _ []     = False
isElementSorted a (x:xs)
  | a == x               = True
  | a > x                = False
  | otherwise            = isElementSorted a xs

-- precondition: list is sorted in increasing order
-- postcondition: return list is sorted in increasing order
insertSorted :: (Eq a, Ord a)  => a -> [a] -> [a]
insertSorted x []     = [x]
insertSorted x (y:ys)
  | x <= y            = x : y : ys
  | otherwise         = y : insertSorted x ys

-- Section 2: Binary trees

-- data BinaryTree a = 
--     Leaf
--     | Node a (BinaryTree a) (BinaryTree a)

data BTree a where
    Leaf :: BTree a
    Node :: a -> BTree a -> BTree a -> BTree a 
    deriving (Show)

-- Section 3: Binary search
-- precondition: tree is sorted in increasing order
-- postcondition: return tree is sorted in increasing order
insertTree :: Ord a => a -> BTree a -> BTree a
insertTree x Leaf = Node x Leaf Leaf
insertTree newValue (Node nodeValue leftSubtree rightSubtree)
    | newValue < nodeValue = 
        Node nodeValue (insertTree newValue leftSubtree) rightSubtree
    | otherwise            = 
        Node nodeValue leftSubtree (insertTree newValue rightSubtree)

-- precondition: tree is sorted in increasing order
isElementTree :: Ord a => a -> BTree a -> Bool
isElementTree x Leaf = False
isElementTree value (Node nodeValue leftSubtree rightSubtree)
  | value == nodeValue  = True     
  | value  <  nodeValue = isElementTree value leftSubtree 
  | otherwise           = isElementTree value rightSubtree

-- Section 4: Balancing
tree1, tree2 :: BTree Integer
tree1 = insertTree 5 $ 
            insertTree 7 $ 
                insertTree 1 $ 
                    insertTree 3 $ 
                        insertTree 6 $ 
                            insertTree 2 $ 
                                insertTree 4 Leaf
tree2 = insertTree 7 $ 
            insertTree 6 $ 
                insertTree 5 $ 
                    insertTree 4 $ 
                        insertTree 3 $ 
                            insertTree 2 $ 
                                insertTree 1 Leaf