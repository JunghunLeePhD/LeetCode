module Card.Data where

type ID = Int
type Opened = Bool
data Details = Details deriving (Show)
data Card = 
    Card {cid :: ID, opened :: Opened, details :: Details} 
    deriving (Show)

