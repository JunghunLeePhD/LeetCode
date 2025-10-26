-- ./module AlgDataType/Basic.hs
module AlgDataType.Basic where

-- Example 1: data synonyms and sum data type
-- Problem
type Point = (Float, Float)
type Line  = (Point, Point)
type LineStyle = String
type FancyLine = (Point, Point, LineStyle)

isRightLineStyle :: FancyLine -> FancyLine
isRightLineStyle line@(_, _, lstyle)
    | lstyle `elem` ["solid", "dashed", "dotted"] = line
    | otherwise = 
        error $ "error in isRightLineStyle: " ++ lstyle ++ " is not a valid style"

myLine, myLine' :: FancyLine
myLine = isRightLineStyle ((0, 0), (1, 1), "dashed") 
myLine' = isRightLineStyle ((0, 0), (1, 1), "bashed")

changeLineStyle :: FancyLine -> LineStyle -> FancyLine
changeLineStyle (x, y, _) newStyle
    | newStyle `elem` ["solid", "dashed", "dotted"] = 
        (x, y, newStyle)
    | otherwise = 
        error $ "error in changeLineStyle: " ++ newStyle ++ " is not a valid style"

newLine, newLine' :: FancyLine
newLine = changeLineStyle myLine "solid" 
newLine' = changeLineStyle myLine "solidd" 

-- Solution
data LineStyleA = Solid | Dashed | Dotted deriving (Show)
type FancyLineA = (Point, Point, LineStyleA)
myLineA :: FancyLineA
myLineA = ((0, 0), (1, 1), Dashed)
-- myLineA' :: FancyLineA
-- myLineA' = ((0, 0), (1, 1), Bashed)  -- 'type' error so do not need to check function
changeLineStyleA :: FancyLineA -> LineStyleA -> FancyLineA
changeLineStyleA (x, y, _) newStyle = (x, y, newStyle)

newLineA :: FancyLineA
newLineA = changeLineStyleA myLineA Solid
-- newLineA' :: FancyLineA
-- newLineA' = changeLineStyleA myLineA Solidd -- 'type' error so ...