-- ./RWH/Ch01.hs
module RWH.Ch01 where

-- Example 1: Word Counter
text1 :: String
text1 = "Teignmouth, England Paris, France Ulm, Germany Auxerre, France Brunswick, Germany Beaumont-en-Auge, France Ryazan, Russia"

wc :: String -> Int
wc = length . words

mywc :: String -> Int
mywc = mylength . mywords

mylength :: [a] -> Int
mylength = foldl (\acc _ -> 1 + acc) 0
-- mylength xs = foldl (.) id ((\_ y -> 1 + y) <$> xs) $ 0
-- mylength [] = 0
-- mylength (x:xs) = 1 + mylength xs

mywords :: String -> [String]
mywords [] = []
mywords xs =
    let
        (pre,suf) = mybreak xs :: (String, String)
    in
        pre: mywords suf

mybreak :: String -> (String, String)
mybreak "" = ("","")
mybreak xs = myfst . mysteps $ xs

myfst :: (a, b) -> a
myfst (one,two) = one

mysteps :: String -> ((String, String), Maybe Char)
mysteps = steps ("",Nothing)
    where
        steps :: (String, Maybe Char) -> String -> ((String, String), Maybe Char)
        steps (this,that) ""  = ((this,""),that)
        steps (xs,char) (y:ys)  =
            case char of
                Just ' '        -> ((xs,y:ys),char)
                _               -> steps (xs++[y],Just y) ys

-- uncurriedstep :: ((String, String), Maybe Char) -> ((String, String), Maybe Char)
-- uncurriedstep ((this,""),that) = ((this,""),that)
-- uncurriedstep ((xs,y:ys),char) = 
--     case char of 
--         Just ' '        -> ((xs,y:ys),char)
--         _               -> ((xs++[y],ys),Just y)

-- uncurriedsteps :: ((String, String), Maybe Char) -> ((String, String), Maybe Char)
-- uncurriedsteps ((this,""),that) = ((this,""),that)
-- uncurriedsteps ((xs,y:ys),char) = 
--     case char of 
--         Just ' '        -> ((xs,y:ys),char)
--         _               -> uncurriedsteps ((xs++[y],ys),Just y)
