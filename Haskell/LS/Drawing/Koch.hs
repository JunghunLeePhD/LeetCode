-- ./Drawing/Koch.hs
module Drawing.Koch where

import Drawing.Basic
    ( Svg(..),
      Polygon(..),
      Styles,
      Style(StrokeWidth, FillOpacity, Stroke),
      Points,
      Point(..),
      Component(csave) )
import Drawing.Path ( Line(..), rotateLine, scaleLine )
import Drawing.Spiral ( connectLine, polygon )

kochLine :: Int -> Point -> Point -> Points
kochLine n ptS ptE
  | n <= 0 = []
  | otherwise = [ptS] 
        ++ kochLine (n - 1) ptS pt1 
        ++ kochLine (n - 1) pt1 pt2
        ++ kochLine (n - 1) pt2 pt3 
        ++ kochLine (n - 1) pt3 ptE 
        ++ [ptE] 
        where 
            l1@(Line _ pt1) = scaleLine (1 / 3) (Line ptS ptE) 
            l2@(Line _ pt3) = connectLine l1 l1 
            Line _ pt2      = rotateLine (5 / 3 * pi) l2

kochFlake :: Int -> Line -> Points
kochFlake n line 
 = kochLine n p1 p2 ++ kochLine n p2 p3 ++ kochLine n p3 p1 
 where
   [p1, p2, p3, _] = polygon 3 line

svgKoch :: IO ()
svgKoch = do
    let 
        kpoints1 :: Points
        kpoints1 = kochFlake 6 (Line (Point2D 640 360) (Point2D 640 0))
        kstyles1 :: Styles
        kstyles1 = [FillOpacity 1, Stroke 255 0 0, StrokeWidth 2]
        kpolygon1 :: Polygon Points Styles
        kpolygon1 = Polygon kpoints1 kstyles1
        kpoints2 :: Points
        kpoints2 = kochFlake 6 (Line (Point2D 640 360) (Point2D 640 720))
        kstyles2 :: Styles
        kstyles2 = [FillOpacity 1, Stroke 0 0 255, StrokeWidth 2]
        kpolygon2 :: Polygon Points Styles
        kpolygon2 = Polygon kpoints2 kstyles2
        kpoints3 :: Points
        kpoints3 = kochFlake 6 (Line (Point2D 640 360) (Point2D 0 360))
        kstyles3 :: Styles
        kstyles3 = [FillOpacity 1, Stroke 255 0 255, StrokeWidth 2]
        kpolygon3 :: Polygon Points Styles
        kpolygon3 = Polygon kpoints3 kstyles3
        ksvg :: Svg Points Styles
        ksvg = Svg [kpolygon1, 
                    kpolygon2,
                    kpolygon3]
    csave "./svg/koch.svg" ksvg