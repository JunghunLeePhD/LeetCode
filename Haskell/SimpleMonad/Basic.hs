module SimpleMonad.Basic where

-- Functor: move points w/o unpackaging explictly
newtype Point = Point1D Float deriving (Show)

pt1 :: Point
pt1 = Point1D 1

move :: Float -> Float
move x = x - 1

pt2 :: Point
pt2 = Point1D . move $ 1

newtype BPoint a = BPoint1D a deriving (Show)

bpt1 :: BPoint Float
bpt1 = BPoint1D 1

bpt2 :: BPoint Float
bpt2 = BPoint1D . move $ 1

instance Functor BPoint where
    fmap :: (a -> b) -> BPoint a -> BPoint b
    fmap f (BPoint1D x) = BPoint1D . f $ x

bpt3 :: BPoint Float
bpt3 = fmap move bpt1

-- Mondad: move point w/ rule
gameover :: Float -> Float
gameover x
    | x > 0 = x
    | otherwise = error "game is over"

game1, game2, game3 :: BPoint Float
game1 = fmap gameover bpt1
game2 = gameover . move <$> bpt1
game3 = gameover . move . move <$> bpt1

mgameover' :: Float -> Maybe Float
mgameover' x
    | x > 0 = Just x
    | otherwise = Nothing

fmmorp :: Float -> BPoint Float
fmmorp = BPoint1D 

mfmorp :: BPoint Float -> Float
mfmorp (BPoint1D x) = x

mgameover :: BPoint Float -> Maybe (BPoint Float)
mgameover = fmap fmmorp . mgameover' . mfmorp

-- mgameover :: BPoint Float -> Maybe (BPoint Float)
-- mgameover (BPoint1D x)
--     | x > 0 = Just . BPoint1D $ x
--     | otherwise = Nothing

mgame1, mgame2, mgame3 :: Maybe (BPoint Float)
mgame1 = mgameover bpt1
mgame2 = mgameover bpt1 >>= mgameover . fmap move
mgame3 = mgameover bpt1 >>= mgameover . fmap move >>= mgameover . fmap move
