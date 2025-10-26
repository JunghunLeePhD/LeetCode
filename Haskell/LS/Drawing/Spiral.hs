module Drawing.Spiral where

import Drawing.Basic
import Drawing.Path ( Line(..), rotateLine, scaleLine )


startLineFrom :: Point -> Line -> Line
startLineFrom (Point2D x y) (Line (Point2D x1 y1) (Point2D x2 y2))
  = Line (Point2D x y) (Point2D (x2 - x1 + x) (y2 - y1 + y))

connectLine :: Line -> Line -> Line
connectLine (Line _ pE) = startLineFrom pE

spiral ::  Float -> Float -> Int -> Line -> [Point]
spiral angle scaleFactor n line =
    spiral' n line 
    where
        spiral' :: Int -> Line -> [Point]
        spiral' n line@(Line pt1 pt2)
                | n <= 0    = []
                | otherwise = 
                    let newLine = 
                            connectLine line . scaleLine scaleFactor . rotateLine angle $ line
                    in pt1 : spiral' (n - 1) newLine

polygon :: Int -> Line -> [Point]
polygon n line | n > 2 = spiral rotationAngle 1 (n + 1) line
  where 
    rotationAngle = (2 * pi) / fromIntegral n

-- Examples
svgSpiral :: IO ()
svgSpiral = do
    let
        spline :: Line
        spline = Line (Point2D 640 360) (Point2D 640 340)
        spline2 :: Line
        spline2 = Line (Point2D 640 360) (Point2D 640 150)
        sppoints1 :: [Point]
        sppoints1 = spiral (2* pi/3) 1.008 1600 spline
        spstyles1 :: [Style]
        spstyles1 = [FillOpacity 0, Stroke 255 0 0, StrokeWidth 2]
        sppolygon1 :: ObjectE [Point] [Style]
        sppolygon1 = PolygonE sppoints1 spstyles1
        sppoints2 :: [Point]
        sppoints2 = spiral 2 1.002 2000 spline
        spstyles2 :: [Style]
        spstyles2 = [FillOpacity 0, Stroke 0 0 255, StrokeWidth 1]
        sppolygon2 :: ObjectE [Point] [Style]
        sppolygon2 = PolygonE sppoints2 spstyles2
        popoints3 :: [Point]
        popoints3 = polygon 5 spline2
        spstyles3 :: [Style]
        spstyles3 = [FillOpacity 0, Stroke 255 255 255, StrokeWidth 5]
        sppolygon3 :: ObjectE [Point] [Style]
        sppolygon3 = PolygonE popoints3 spstyles3
        picture :: Svg [ObjectE [Point] [Style]]
        picture = Svg [
            sppolygon1 
            , sppolygon2 
            , sppolygon3
                        ]
    csave "./svg/spirals.svg" picture