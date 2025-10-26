-- ./HiOrd/Flexibility.hs
module HiOrd.Flexibility where

import Drawing.Basic
import Drawing.Path
import Drawing.Spiral
import Text.PrettyPrint (style)
-- fade :: Color -> Color
-- fade col@(redC, greenC, blueC, opacityC)
--     | opacityC == 0 = col
--     | otherwise     = (redC, greenC, blueC, opacityC - 20)

-- colouredFTree :: Int -> Float -> Color -> Line -> Picture
-- colouredFTree n factor colour line = fT n colour line 
--   where
--     fT 0 colour line = [(colour, [fst line, snd line])]  
--     fT n colour line = [(colour,[p2, p3])]  
--                        ++ [(colour,[p4, p1])] 
--                        ++ fT (n-1) colour' (p5, p3) 
--                        ++ fT (n-1) colour' (p4, p5)
--       where 
--         colour'         = fade colour
--         [p1,p2,p3,p4,_] = polygon 4 line
--         (_, p5)         = rotateLine (factor * pi)
--                             $ (\(x,y) -> (y,x)) 
--                             $ scaleLine 0.5 (p3, p4)


fade :: Style -> Style
fade (Opacity op) 
    | op == 0   = Opacity op
    | otherwise = Opacity (op - 0.2)
fade style = style

coloredFTree :: Int -> Float -> Styles -> Line -> Polygons Line Styles
coloredFTree n factor styles line = fT n styles line 
    where
        fT :: Int -> Styles -> Line -> Polygons Line Styles
        fT 0 styles line = [Polygon line styles]  
        fT n styles line = [Polygon (Line p2 p3) styles]
                            ++ [Polygon (Line p4 p1) styles]
                            ++ fT (n-1) newStyles (Line p5 p3)
                            ++ fT (n-1) newStyles (Line p4 p3)
            where
                newStyles :: Styles
                newStyles = fmap fade styles 
                p1, p2, p3, p4 :: Point
                [p1, p2, p3, p4, _] = polygon 4 line
                p5 :: Point
                Line _ p5 = rotateLine (factor * pi)
                                $ (\(Line x y) -> Line y x)
                                $ scaleLine 0.5 (Line p3 p4)



svgFTree :: IO ()
svgFTree = do
    let
        line1 :: Line
        line1 = Line (Point2D 640 360) (Point2D 640 340)
        line2 :: Line
        line2 = Line (Point2D 640 360) (Point2D 640 380)
        line3 :: Line
        line3 = Line (Point2D 640 360) (Point2D 600 360)
        styles1 :: Styles
        styles1 = [FillOpacity 0, Stroke 255 0 0, StrokeWidth 2]
        styles2 :: Styles
        styles2 = [FillOpacity 0, Stroke 0 0 255, StrokeWidth 1]
        styles3 :: Styles
        styles3 = [FillOpacity 0, Stroke 255 255 255, StrokeWidth 5]
        tf1, tf2, tf3 :: Polygons Line Styles
        tf1 = coloredFTree 30 0.3 styles1 line1
        tf2 = coloredFTree 30 0.5 styles2 line2
        tf3 = coloredFTree 30 0.1 styles3 line3
        picture :: Svg Line Styles
        picture = Svg $ tf1 ++ tf2 ++ tf3
    csave "./svg/coloredFTree.svg" picture