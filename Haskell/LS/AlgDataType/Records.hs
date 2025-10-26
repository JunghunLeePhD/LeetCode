-- ./AlgDataType/Records.hs
module AlgDataType.Records where

import Data.List ( intercalate )
import Drawing.Basic
import AlgDataType.Sum

-- Section 1: Projection functions and field names
data Point' = 
    Point2D' { x :: Float
            , y :: Float } 
    deriving (Show)

data Vector' = 
    Vector2D'{ vx :: Float
            , vy :: Float } 
    deriving (Show)

movePoint' :: Vector' -> Point' -> Point'
movePoint' vector' point' = 
    Point2D' (x point' + vx vector') (y point' + vy vector')

instance Property Point' where
    pshow :: Point' -> String
    pshow point' = show (x point') ++ "," ++ show (y point')
instance Properties [Point'] where
    psshow :: [Point'] -> String
    psshow as = 
        "points=\"" ++ (intercalate " " . fmap pshow $ as) ++ "\""

point' :: IO ()
point' = do
    let 
        point1 = Point2D' {x = 1, y = 2}
        point2 = Point2D' 3 4
        vector1 = Vector2D' {vx = 100, vy = 200}
        vector2 = Vector2D' 100 200
    putStrLn $ psshow [point1, point2]
    putStrLn $ psshow [movePoint' vector1 point1, movePoint' vector2 point2]

-- Section 2: Record updates



-- Section 3: Total versus partial projections

-- Section 4: A more complex example
data TKind = 
    TNo 
    | TRotate Point Float 
    | TTrans Vector 
    -- | TScale Vector 
    deriving (Show)
data TObjectE a b = TObjectE TKind (ObjectE a b) deriving (Show)

instance (Properties a, Properties b) => Element (TObjectE a b) where
    eshow :: TObjectE a b -> String
    eshow (TObjectE tkind objecte) =
        case tkind of
            TNo -> "<g>" ++ eshow objecte ++ tetail
            TRotate point angle -> trehead point angle ++ eshow objecte ++ tetail
            TTrans vector -> ttehead vector ++ eshow objecte ++ tetail
            -- TScale vector -> tsehead vector ++ eshow objecte ++ tetail
        where
            tetail :: String
            tetail = "</g>"
            trehead :: Point -> Float -> String
            trehead (Point2D x y) angle = 
                "<g transform=\"rotate(" ++ show angle ++ " " 
                ++ show x ++ " " ++ show y ++ ")\">"
            ttehead :: Vector -> String
            ttehead (Vector2D vx vy) = "<g transform=\"translate(" ++ show vx ++ " " ++ show vy ++ ")\">"
            -- tsehead :: Vector -> String
            -- tsehead (Vector2D vx vy) = "<g transform=\"scale(" ++ show vx ++ " " ++ show vy ++ ")\">"

rotationSvg :: IO ()
rotationSvg = do
    let
        center :: Point
        center = Point2D 640 360
        angleo :: Float
        angleo = 0
        angle :: Float
        angle = 100
        greenEllipse :: ObjectE ObjectP [Style]
        greenEllipse   = 
            EllipseE
                (EllipseP center 100 200 0) 
                [Fill "#00ff00", FillOpacity 0, Stroke 0 255 0]

        greenEllipseTn :: TObjectE ObjectP [Style]
        greenEllipseTn  = TObjectE TNo greenEllipse
        greenEllipseTr :: TObjectE ObjectP [Style]
        greenEllipseTr  = TObjectE (TRotate center angle) greenEllipse
        greenEllipseTt :: TObjectE ObjectP [Style]
        greenEllipseTt  = TObjectE (TTrans (Vector2D 100 50)) greenEllipse
        -- greenEllipseTs :: TObjectE ObjectP [Style]
        -- greenEllipseTs  = TObjectE (TScale (Vector2D 0.5 1)) greenEllipse
        svg :: Svg [TObjectE ObjectP [Style]]
        svg = Svg [greenEllipseTn, greenEllipseTr, greenEllipseTt]
        
    csave "./svg/rotation.svg" svg
            
magnetaSvg :: IO ()
magnetaSvg = do
    let
        center :: Point
        center = Point2D 640 360
        magnetaPoly :: ObjectE ObjectP [Style]
        magnetaPoly = 
            PolygonE 
                (PolygonP [Point2D 700 360, Point2D 800 380, Point2D 1000 360, Point2D 800 340])
                [Fill "#990099", FillOpacity 0.5, Stroke 153 0 153, Opacity 1]
        magentaPolys :: [TObjectE ObjectP [Style]]
        magentaPolys = 
            fmap 
                (\n -> TObjectE (TRotate center (0 + 5*n)) magnetaPoly) 
                [1..72]

        svg :: Svg [TObjectE ObjectP [Style]]
        svg = Svg magentaPolys
        
    csave "./svg/magneta.svg" svg


-- dynamicSvg :: IO ()
-- dynamicSvg = do
--     let
--         center :: Point
--         center = Point2D 640 360
--         magnetaPoly :: ObjectE ObjectP [Style]
--         magnetaPoly = 
--             PolygonE 
--                 (PolygonP [Point2D 700 360, Point2D 800 380, Point2D 1000 360, Point2D 800 340])
--                 [Fill "#990099", FillOpacity 0.5, Stroke 153 0 153, Opacity 1]

--         dynamicRotate:: ObjectE ObjectP [Style] -> Point -> Vector -> Float -> Int -> Picture
--         dynamicRotate objecte _   _   _     0 = [objecte]
--         dynamicRotate objecte pnt vec alpha n = 
--             rotatePicObj : dynamicRotate (movePictureObject vec rotatePicObj)(movePoint vec pnt) vec alpha (n-1)
--         where
--             rotatePicObj = rotatePictureObject alpha pnt picObj

--         -- svg :: Svg [TObjectE ObjectP [Style]]
--         -- svg = Svg magentaPolys
--     -- csave "./svg/dynamics.svg" svg
