-- ./Drawing/Path.hs
module Drawing.Path where

import Drawing.Basic

data Line = Line Point Point deriving (Show)
instance Properties Line where
    psshow :: Line -> String
    psshow (Line pt1 pt2) = psshow [pt1, pt2]
    
rotateLine :: Float -> Line -> Line
rotateLine alpha (Line (Point2D x1 y1) (Point2D x2 y2)) =
    Line (Point2D x1 y1) (Point2D x2' y2')
    where 
        x2' = (x2 - x1) * cos alpha - (y2 - y1) * sin alpha + x1
        y2' = (x2 - x1) * sin alpha + (y2 - y1) * cos alpha + y1

scaleLine :: Float -> Line -> Line
scaleLine factor (Line (Point2D x1 y1) (Point2D x2 y2)) = 
    Line (Point2D x1 y1) (Point2D x2' y2') 
    where 
        x2' = (x2 - x1) * factor + x1
        y2' = (y2 - y1) * factor + y1


fades :: Float -> [Style] -> [Style]
fades factor = fmap fade
    where
        fade :: Style -> Style
        fade (Opacity op) = let op' = op - factor in Opacity op'
        fade style = style

spiralRays :: Float -> Float -> Float -> Int -> [Style] -> Line -> [ObjectE Line [Style]]
spiralRays angle scaleFactor fadeFactor n styles line
  | n <= 0 = []
  | otherwise = PolygonE line styles : spiralRays angle scaleFactor fadeFactor (n-1) newStyles newLine
    where
        newStyles = fades fadeFactor styles
        newLine = scaleLine scaleFactor . rotateLine angle $ line

newSpiralRays :: Float -> Float -> Float -> Int -> [Style] -> Line -> [ObjectE Line [Style]]
newSpiralRays angle scaleFactor fadeFactor n styles line
  | n <= 0 = []
  | otherwise = PolygonE line styles : spiralRays angle scaleFactor fadeFactor (n-1) newStyles newLine
    where
        newStyles = if n `mod` 3 == 0
                       then fades fadeFactor styles
                       else styles
        newLine = scaleLine scaleFactor . rotateLine angle $ line

-- Examples 
line :: Line
line = Line (Point2D 640 360) (Point2D 640 0)
rline :: Line
rline = rotateLine 1 line
sline :: Line
sline = scaleLine 0.5 . rotateLine 2 $ line
fline :: Line
fline = rotateLine 3 line

-- style = Opacity 1
styles :: [Style]
styles = [Fill "#ffffff", Stroke 255 0 0, StrokeWidth 2, Opacity 1]
rstyles :: [Style]
rstyles = [Fill "#ffffff", Stroke 0 0 255, StrokeWidth 4, Opacity 1]
sstyles :: [Style]
sstyles = [Fill "#ffffff", Stroke 255 0 255, StrokeWidth 1, Opacity 1]
fstyles :: [Style]
fstyles = fades 0.01 [Fill "#ffffff", Stroke 0 0 255, StrokeWidth 4, Opacity 1]

svgLine :: IO()
svgLine = do
    let 
        picture = Svg $ spiralRays 0.9 0.999 0.2 6 styles line
    csave "./svg/line.svg" picture
    let 
        newpicture = Svg $ newSpiralRays 0.9 0.999 0.2 6 styles line
    csave "./svg/newline.svg" newpicture
