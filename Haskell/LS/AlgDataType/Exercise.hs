-- ./AlgDataType/Exercise.hs
module AlgDataType.Exercise where


-- Exercise 1: Rewrite the defintion of map
-- map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x : xs) = f x : map f xs
-- to use case notation — i.e., complete the following definition
-- map f xs = case xs of
--            …
fmap' :: (a -> b) -> [a] -> [b]
fmap' f xs = case xs of
    [] -> []
    (x:xs) -> f x : fmap' f xs

-- Exercise 2: Write a function which, 
-- given a day, 
-- returns the data constructor representing the following day:
-- nextDay :: Day -> Day
data Day = 
    Sunday 
    | Monday 
    | Tuesday 
    | Wedsday 
    | Thursday 
    | Friday 
    | Saturday
    deriving (Show, Enum)
nextDay :: Day -> Day
nextDay day = 
    case day of 
        Saturday -> Monday
        _ -> succ day

-- Exercise 3: How would you define a data type 
-- to represent the different cards of a deck of poker cards? 
-- How would you represent a hand of cards?
-- Can you define a function which, 
-- given a hand of cards calculates its values according to the 21-rules: 
-- that is, all the cards from 2 to 10 are worth their face value. 
-- Jack, Queen, King count as 10.
-- The Ace card is worth 11, but if this would mean the overall 
-- value of the hand exceeds 21, it is valued at 1.
data CardType = 
    Clover 
    | Space 
    | Spade 
    | Heart 
    deriving (Show)
data CardNumb =
    Ace | Two | Three | Four | Five | Six | Seven 
    | Eight | Nine | Ten | Jack | Queen | King
    deriving (Show, Enum)
data Card = 
    Card CardType CardNumb 
    deriving (Show)
type Deck = [Card]

cevalNA :: Card -> Int
cevalNA (Card ctype cnumb) = 
    case cnumb of
        Jack -> 10
        Queen -> 10
        King -> 10
        Two -> 2
        Ace -> 0
        _ -> 1 + cevalNA (Card ctype (pred cnumb))
cevalA1 :: Card -> Int
cevalA1 (Card _ cnumb) =
    case cnumb of
        Ace -> 11
        _ -> 0
cevalA2 :: Card -> Int
cevalA2 (Card _ cnumb) =
    case cnumb of
        Ace -> 1
        _ -> 0

deval :: Deck -> Int
deval cards
    | deval1 cards <= 21 = deval1 cards
    | otherwise = deval2 cards
    where 
        deval1 :: Deck -> Int
        deval1 = sum . fmap (\card -> cevalNA card + cevalA1 card)
        deval2 :: Deck -> Int
        deval2 = sum . fmap (\card -> cevalNA card + cevalA2 card)

-- Exercise 4: Can you define a function 
-- simpleCirclePic :: Float -> Picture
-- , similar to simpleEllipsePic that we defined earlier,
-- such that it generates a picture of circles, 
-- which increase in size in dependence on the floating point argument,
-- such that the radius of the biggest circle does not exceed 400.
-- The generated pictures should look like those in the following display.

