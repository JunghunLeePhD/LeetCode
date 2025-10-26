-- ./AlgDataType/Sum.hs
module AlgDataType.Sum where

import Drawing.Basic
import Drawing.Path

-- Section 1: Introduction
lambdaPoints :: [Point]
lambdaPoints = [Point2D 210 200, Point2D 270 200, Point2D 545 600,
             Point2D 525 600, Point2D 380 390, Point2D 250 600,
             Point2D 230 600, Point2D 370 380, Point2D 260 215,
             Point2D 210 215]

lambdaStyles, lambdaStyles2 :: [Style]
lambdaStyles = [Fill "#ffffff", FillOpacity 1, Stroke 255 0 0, StrokeWidth 4, Opacity 1]
lambdaStyles2 = [Fill "#000000", FillOpacity 1, Stroke 255 0 0, StrokeWidth 4, Opacity 1]

lambdaSvg :: IO ()
lambdaSvg = do
    let
        svg = Svg [PolygonE lambdaPoints lambdaStyles]
        svg2 = Svg [PolygonE lambdaPoints lambdaStyles2]
    csave "./svg/lambda.svg" svg
    csave "./svg/lambda2.svg" svg2

circlep, circlep2, circlep3:: ObjectP
circlep = CircleP (Point2D 640 360) 300
circlep2 = CircleP (Point2D 640 360) 200
circlep3 = CircleP (Point2D 640 360) 50


cricleStyles, cricleStyles2, cricleStyles3 :: [Style]
cricleStyles = [Fill "#000000", FillOpacity 0, Stroke 0 0 255, StrokeWidth 4, StrokeDash 9, Opacity 1]
cricleStyles2 = [Fill "#000000", FillOpacity 0, Stroke 0 255 0, StrokeWidth 4, StrokeDash 4, Opacity 1]
cricleStyles3 = [Fill "#ff0000", FillOpacity 1, Stroke 255 0 0, StrokeWidth 4, Opacity 0.5, Opacity 1]


circleSvg :: IO ()
circleSvg = do
    let
        svg = Svg [CircleE circlep cricleStyles,
                   CircleE circlep2 cricleStyles2,
                    CircleE circlep3 cricleStyles3
                   ]
    csave "./svg/circle.svg" svg

redEllipse, greenEllipse, blueEllipse:: ObjectP
redEllipse = EllipseP (Point2D 640 360) 400 200 135
greenEllipse = EllipseP (Point2D 640 360) 300 100 45
blueEllipse = EllipseP (Point2D 640 360) 200 50 90

redEllipseS, greenEllipseS, blueEllipseS :: [Style]
redEllipseS = [Fill "#000000", FillOpacity 0, Stroke 255 0 0, StrokeWidth 4, StrokeDash 9, Opacity 1]
greenEllipseS = [Fill "#000000", FillOpacity 0, Stroke 0 255 0, StrokeWidth 4, StrokeDash 4, Opacity 1]
blueEllipseS = [Fill "#0000ff", FillOpacity 1, Stroke 0 0 255, StrokeWidth 4, Opacity 1]

ellipseSvg :: IO ()
ellipseSvg = do
    let
        -- svg :: Svg [ObjectP [Style]]
        svg = Svg [EllipseE redEllipse redEllipseS,
                   EllipseE greenEllipse greenEllipseS,
                    EllipseE blueEllipse blueEllipseS
                   ]
    csave "./svg/ellipse.svg" svg

-- Section 2: Generating pictures
simpleEllipses :: Float -> [ObjectE ObjectP [Style]]
simpleEllipses n = fmap greenEllipse [0, pi/n..(n-1) * pi/n]
  where
    -- greenEllipse :: Float -> ObjectP [Style]
    greenEllipse angle = EllipseE
                        (EllipseP (Point2D 640 360) 300 100 angle)
                        [Fill "#00ff00",
                         FillOpacity 0.2,
                         Stroke 0 255 0,
                         StrokeWidth 1,
                         StrokeWidth 0.3]

simpleEllipsesSvg :: IO()
simpleEllipsesSvg = do
    let
        -- svg :: Svg [ObjectP [Style]]
        svg = Svg (simpleEllipses 5)
    csave "./svg/simpleellipses.svg" svg


-- Section 3: Transforming pictures
data Vector = Vector2D Float Float deriving (Show)

movePoint :: Vector -> Point -> Point
movePoint (Vector2D vx vy) (Point2D x y) = 
    Point2D (x+vx) (y+vy)
    -- Point2D (x point+vx vector) (y point+vy vector)

moveObjectP :: Vector -> ObjectP -> ObjectP
moveObjectP vector objectp = case objectp of
    EllipseP point rx ry angle -> EllipseP (movePoint vector point) rx ry angle
    CircleP point r            -> CircleP (movePoint vector point) r
    PolygonP points            -> PolygonP (fmap (movePoint vector) points)

moveObjectE :: Vector -> ObjectE ObjectP b -> ObjectE ObjectP b
moveObjectE vector objecte = case objecte of
    EllipseE ellipsep styles -> EllipseE (moveObjectP vector ellipsep) styles
    CircleE circlep styles   -> CircleE (moveObjectP vector circlep) styles
    PolygonE polygonp styles -> PolygonE (moveObjectP vector polygonp) styles

transformingSvg :: IO ()
transformingSvg = do
    let
        moveobject1, moveobject2 :: ObjectE ObjectP b -> ObjectE ObjectP b
        moveobject1 = moveObjectE (Vector2D 250 0)
        moveobject2 =  moveObjectE (Vector2D 150 0)
        objectes :: [ObjectE ObjectP [Style]]
        objectes = [
            CircleE circlep3 cricleStyles3,
            moveobject1 $ CircleE circlep cricleStyles,
            moveobject2 $ CircleE circlep2 cricleStyles2
                   ]
        svg :: Svg [ObjectE ObjectP [Style]]
        svg = Svg objectes
    csave "./svg/transforming.svg" svg


curvesSvg :: IO ()
curvesSvg = do
    let
        redCircle :: ObjectE ObjectP [Style]
        redCircle      = 
            CircleE 
                (CircleP (Point2D 20 360) 10) 
                [Fill "#ff0000", FillOpacity 1, Stroke 255 0 0]
        greenEllipse :: ObjectE ObjectP [Style]
        greenEllipse   = 
            EllipseE
                (EllipseP (Point2D 20 360) 5 30 0) 
                [Fill "#00ff00", FillOpacity 1, Stroke 0 255 0]
        whitePolygon :: ObjectE ObjectP [Style]
        whitePolygon   = 
            PolygonE 
                (PolygonP [Point2D 20 360, Point2D 40 360, Point2D 40 420])
                [Fill "#ffffff", FillOpacity 1, Stroke 255 255 255]
        bluePath :: ObjectE ObjectP [Style]
        bluePath       = 
            PolygonE
                (PolygonP [Point2D 20 360, Point2D 40 360, Point2D 40 420])
                [Fill "#0000ff", FillOpacity 1, Stroke 0 0 255]
        xvals :: [Float]
        xvals          = 
            [0,5..1280]

        makeCircleSin, makePolygonCos, makeEllipseSin, makePathCos:: Float -> ObjectE ObjectP [Style]
        makeCircleSin x = 
            moveObjectE (Vector2D x $ 100 * sin (pi * x/200)) redCircle
        makePolygonCos x = 
            moveObjectE (Vector2D x $ 100 * cos (pi * x/200)) whitePolygon
        makeEllipseSin x =
            moveObjectE (Vector2D x $ 250 * sin (pi * x/200)) greenEllipse
        makePathCos    x = 
            moveObjectE (Vector2D x $ 200 * cos (pi * x/200)) bluePath

        circleSin, polygonCos, ellipseSin, pathCos:: [ObjectE ObjectP [Style]]
        circleSin = fmap makeCircleSin xvals
        polygonCos = fmap makePolygonCos xvals
        ellipseSin = fmap makeEllipseSin xvals
        pathCos = fmap makePathCos xvals

        svg :: Svg [ObjectE ObjectP [Style]]
        svg = Svg $ 
            circleSin 
            ++ polygonCos 
            ++ ellipseSin 
            ++ pathCos

    csave "./svg/curves.svg" svg