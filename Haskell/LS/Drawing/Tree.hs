module Drawing.Tree where

import Drawing.Basic
import Drawing.Path
import Drawing.Spiral

fractalTree :: Float -> Int -> Line -> Points
fractalTree factor n line = fractalTree' n line 
    where 
        fractalTree' :: Int -> Line -> Points
        fractalTree' 0 line = []  
        fractalTree' n line = [p1, p4] 
            ++ fractalTree' (n-1) (Line p4 p5)
            ++ fractalTree' (n-1) (Line p5 p3) 
            ++ [p3, p2] 
            where 
                flipLine :: Line -> Line 
                flipLine (Line pS pE) = Line pE pS
                [p1,p2,p3,p4,_] = polygon 4 line
                r               = flipLine . scaleLine 0.5 $ Line p3 p4
                Line _ p5       = rotateLine (factor * pi) r 

svgTree :: IO ()
svgTree = do
    let
        tpoints1 :: Points
        tpoints1 = fractalTree (1/4) 15 (Line (Point2D 640 360) (Point2D 640 300))
        tstyles1 :: Styles
        tstyles1 = [FillOpacity 0, Stroke 255 0 0, StrokeWidth 2]
        tpolygon1 :: Polygon Points Styles
        tpolygon1 = Polygon tpoints1 tstyles1 
        tpoints2 :: Points
        tpoints2 = fractalTree (2/3) 15 (Line (Point2D 640 360) (Point2D 580 360))
        tstyles2 :: Styles
        tstyles2 = [FillOpacity 0, Stroke 0 0 255, StrokeWidth 2]
        tpolygon2 :: Polygon Points Styles
        tpolygon2 = Polygon tpoints2 tstyles2 
        tsvg :: Svg Points Styles
        tsvg = Svg [tpolygon1
                    ,tpolygon2] 
    csave "./svg/tree.svg" tsvg