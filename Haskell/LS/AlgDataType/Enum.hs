-- ./AlgDataType/Enum.hs
module AlgDataType.Enum where

-- As sum in type-category: (..((Void + Sunday) + Monday) ... + Saturday)
--                 ...
--        Void | Sunday | Monday    ...
--         /         \
--        /           \
--  Void | Sunday    Monday
--     /   \
--   Void Sunday
data Day
    = Sunday
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday 
    -- deriving (Enum, Show)
    deriving (Enum, Show, Eq)

-- Case 1: Pattern matching and case expressions

isWeekday :: Day -> Bool
isWeekday Sunday   = False
isWeekday Saturday = False
isWeekday _        = True

-- Need to be Eq class
isWeekday' :: Day -> Bool
isWeekday' day
    | day == Sunday   = False
    | day == Saturday = False
    | otherwise       = True

isWeekday'' :: Day -> Bool
isWeekday'' day = case day of
    Sunday   -> False
    Saturday -> False
    _        -> True

isWeekday''' :: Day -> Bool
isWeekday''' day = case day of {Sunday -> False ; Saturday -> False; _ -> True}

-- Case 2: Deriving type classes

-- Need to be Eq class
isWeekday2 :: Day -> Bool
isWeekday2 day = not $ day `elem` [Saturday, Sunday]
