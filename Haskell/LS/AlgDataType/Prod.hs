-- ./AlgDataType/Prod.hs
module AlgDataType.Prod where

-- Problem
type Point   = (Float, Float)
type Vector  = (Float, Float)

movePointN :: Float -> Vector -> Point -> Point
movePointN n (vx, vy) (x, y) = (n * vx + x, n * vy + y)

point :: Point
point = (1, 1)
vector :: Vector
vector = (2, 2)
intend, unintend :: Point
intend =  movePointN 5 vector point
unintend = movePointN 5 point vector

-- Solution
data BPoint = BPoint Float Float deriving (Show)
data BVector = BVector Float Float deriving (Show)

bmovePointN :: Float -> BVector -> BPoint -> BPoint
bmovePointN n (BVector vx vy) (BPoint x y) = BPoint (n * vx + x) (n * vy + y)

bpoint :: BPoint
bpoint = BPoint 1 1
bvector :: BVector
bvector = BVector 2 2
bintend :: BPoint
bintend = bmovePointN 5 bvector bpoint

-- bunintend :: BPoint
-- bunintend = bmovePointN 5 bpoint vector -- type error

morpPtoBp :: Point -> BPoint
morpPtoBp (x, y) = BPoint x y

morpBPtoP :: BPoint -> Point
morpBPtoP (BPoint x y) = (x, y)

isId1 :: BPoint -> BPoint
isId1 = morpPtoBp . morpBPtoP 
isId2 :: Point -> Point
isId2 = morpBPtoP . morpPtoBp 