{-
    @About
        Haskell/Applicative functors
    @See
        https://en.m.wikibooks.org/wiki/Haskell/Applicative_functors
-}

module WikiBooks.Ch05S10 where

-- Exercises

data Tree a = Node a [Tree a] deriving (Show)

tree1, tree2, tree3, tree4, tree5 :: Tree Int
tree1 = Node 1 []
tree2 = Node 1 [Node 2 []]
tree3 = Node 1 [Node 2 [], Node 3 []]
tree4 = Node 1 [Node 2 [Node 4 []], Node 3 [Node 5 []]]
tree5 = Node 1 [Node 2 [Node 4 []], Node 3 [Node 5 []], Node 6 []]

instance Functor Tree where
    fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Node x []) = Node (f x) []
    fmap f (Node x trees) = Node (f x) (fmap f <$> trees)