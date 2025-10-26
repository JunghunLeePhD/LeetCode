-- ./AlgDataType/Wild.hs
module AlgDataType.Wild where

data Color = 
    Color {
        redC :: Int
        , greenC :: Int
        , blueC :: Int
        }
    deriving (Show)

greenComponent :: Color -> Int
greenComponent Color{greenC = green} = green

greenComponent' :: Color -> Int
greenComponent' = greenC

greenComponent'' :: Color -> Int
greenComponent'' Color{greenC} = greenC    -- with punning

greenComponent''' :: Color -> Int
greenComponent''' Color{greenC = greenC} = greenC   -- without punning

clr1, clr2, clr3 :: Color
clr1 = Color 255 0 0 
clr2 = Color {redC = 0, greenC = 255, blueC = 0}
clr3 = Color{redC, greenC, blueC}
    where
        redC     = 0
        greenC   = 127 
        blueC    = 0
