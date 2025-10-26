-- ./MoreAlgDataType.RecursiveDataType.hs
module MoreAlgDataType.RecursiveDataType where

data List' a where
    Cons' :: a -> List' a -> List' a
    Nil'  ::                 List' a
    deriving (Show)

list'1 :: List' Integer
list'1 = Cons' 1 $ Cons' 2 Nil'

-- list'2 = Cons' (Cons' (Cons' (Cons' 4 Nil') 3) 2) 1