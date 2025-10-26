-- ./Drawing/Basic.hs
module Drawing.Basic where

import Data.List (intercalate)

class Property a where
    pshow :: a -> String
class Properties b where
    psshow :: b -> String
class Element a where
    eshow :: a -> String
class Elements b where
    esshow :: b -> String
type FileName = String
class Component a where
    cshow :: a -> String
    csave :: FileName -> a -> IO ()
    csave filename content = do
        writeFile filename . cshow $ content


data Style = 
    Fill String 
    | FillOpacity Float 
    | Stroke Int Int Int 
    | StrokeWidth Float 
    | StrokeDash Int
    | Opacity Float 
    deriving (Show)
instance Property Style where
    pshow :: Style -> String
    pshow (Fill colorcode) = "fill:" ++ colorcode ++ ";"
    pshow (FillOpacity op) = "fill-opacity:" ++ show op ++ ";"
    pshow (Stroke r g b) = "stroke:" ++ "rgb" ++ show (r,g,b) ++ ";"
    pshow (StrokeWidth width) = "stroke-width:" ++ show width ++ ";"
    pshow (StrokeDash dash) = "stroke-dasharray:" ++ show dash ++ ";"
    pshow (Opacity op) = "opacity:" ++ show op ++ ";"

instance Properties [Style] where
    psshow :: [Style] -> String
    psshow as = 
        "style=\"" 
        ++ (intercalate "" . fmap pshow $ as) 
        ++ "\""

data Point = Point2D Float Float deriving (Show)

instance Property Point where
    pshow :: Point -> String
    pshow (Point2D x y) = show x ++ "," ++ show y
instance Properties [Point] where
    psshow :: [Point] -> String
    psshow as = 
        "points=\"" ++ (intercalate " " . fmap pshow $ as) ++ "\""

data ObjectP = 
    EllipseP Point Float Float Float
    | CircleP Point Float 
    | PolygonP [Point]
    deriving (Show)

instance Properties ObjectP where
    psshow :: ObjectP -> String
    psshow (EllipseP (Point2D x y) rx ry angle) = "cx=\"" ++ show x ++ "\"" ++ " cy=\"" ++ show y 
        ++ "\"" ++ " rx=\"" ++ show rx ++ "\"" ++ " ry=\"" ++ show ry ++ "\""
        ++ " transform=\"rotate(" ++ show (180 * angle / pi) ++ "," ++ show x ++ ","
        ++ show y ++ ")\""
    psshow (CircleP (Point2D x y) r) = "cx=\"" ++ show x ++ "\"" ++ " cy=\"" ++ show y ++ "\""
        ++ " r=\"" ++ show r ++ "\""
    psshow (PolygonP as) = 
        "points=\"" ++ (intercalate " " . fmap pshow $ as) ++ "\""

data ObjectE a b = 
    EllipseE a b 
    | CircleE a b 
    | PolygonE a b
    deriving (Show)

instance (Properties a, Properties b) => Element (ObjectE a b) where
    eshow :: ObjectE a b -> String
    eshow (EllipseE as bs) = "<ellipse " ++ psshow as ++ " " ++ psshow bs ++ " />"
    eshow (CircleE as bs) = "<circle " ++ psshow as ++ " " ++ psshow bs ++ " />"
    eshow (PolygonE as bs) = "<polygon " ++ psshow as ++ " " ++ psshow bs ++ " />"

instance (Element e) => Elements [e] where
    esshow :: [e] -> String
    esshow = intercalate "" . fmap eshow

newtype Svg es = Svg es deriving (Show)
instance  (Elements es) => Component (Svg es) where
    cshow :: Svg es -> String
    cshow (Svg elements) = 
        "<svg style=\"background-color:black\" xmlns=\"http://www.w3.org/2000/svg\">"  
        ++ esshow elements 
        ++
        "</svg>"
