-- ./MoreAlgDataType.ParaDataType.hs
module MoreAlgDataType.ParaDataType where

showFifthElement :: Show a => [a] -> String
showFifthElement xs
  = if length xs < 5
      then "there is no fifth element in this list"
      else "the fifth element of the list is: " ++ show (xs !! 4)

(!+!) :: [a] -> Int -> Maybe a
[]      !+! _ = Nothing
(x : _) !+! 0 = Just x
(_ :xs) !+! n = xs !+! (n - 1)

showFifthElement' :: Show a => [a] -> String
showFifthElement' xs
  = case xs !+! 4 of
      Nothing -> "there is no fifth element in this list"
      Just n  -> "the fifth element of the list is: " ++ show n

data Maybe' a where
    Just'     :: a -> Maybe' a
    Nothing'  ::      Maybe' a