-- ./AlgDataType/ConsFunc.hs
module AlgDataType.ConsFunc where


data Point3D where
    Point3D :: Float -> Float -> Float -> Point3D
    deriving (Show)

data Colour where 
    RGB :: Int -> Int -> Int -> Colour
    HEX :: String -> Colour
    deriving (Show)
    
data Arrow where
    Dashed :: Arrow
    Dotted :: Arrow
    deriving (Show)

data Object3D where
    Line :: Point3D -> Point3D -> Colour -> Arrow -> Object3D
    Triangle :: Point3D -> Point3D -> Point3D -> Colour -> Arrow -> Object3D
    deriving (Show)


pt1, pt2, pt3 :: Point3D
pt1 = Point3D 1 1 1
pt2 = Point3D 2 2 2
pt3 = Point3D 3 3 3
clr1, clr2  :: Colour
clr1 = RGB 255 0 0
clr2 = HEX "#ff0000"

arr1, arr2 :: Arrow
arr1 = Dashed
arr2 = Dotted

obj1, obj2 :: Object3D
obj1 = Line pt1 pt2 clr1 arr1
obj2 = Triangle pt1 pt2 pt3 clr2 arr2

explain :: Object3D -> String
explain obj = 
    case obj of
        Line pt1 pt2 clr arr -> 
            "It is a line from " ++ show pt1 ++ "to" ++ show pt2
            ++ "with color is " ++ show clr ++ "with " ++ show arr ++ "line."
        Triangle pt1 pt2 pt3 clr arr -> 
            "It is a triangle from " ++ show pt1 ++ "and" ++ show pt2 ++ "to" ++ show pt3 
            ++ "with color is " ++ show clr ++ "with " ++ show arr ++ "line."