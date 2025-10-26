{-
    @About
        Haskell/Alternative and MonadPlus
    @See
        https://en.m.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus
-}
module WikiBooks.Ch05 where
import Control.Monad hiding (Alternative, MonadPlus, mzero, mplus, msum)
import Prelude hiding (Monoid)

class (Applicative f) => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    asum :: (Foldable t) => t (f a) -> f a
    asum = foldr (<|>) empty

instance Alternative Maybe where
    empty :: Maybe a
    empty               = Nothing
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> Nothing = Nothing -- 0 results + 0 results = 0 results
    Just x  <|> Nothing = Just x  -- 1 result  + 0 results = 1 result
    Nothing <|> Just x  = Just x  -- 0 results + 1 result  = 1 result
    Just x  <|> Just y  = Just x  -- 1 result  + 1 result  = 1 result:
                                -- Maybe can only hold up to one result,
                                -- so we discard the second one.

instance Alternative [] where
    empty :: [a]
    empty = []
    (<|>) :: [a] -> [a] -> [a]
    (<|>) = (++) -- length xs + length ys = length (xs ++ ys)

digit :: (Ord a, Num a, Show a) => a -> [Char] -> Maybe a
digit i (c:_)
    | i < 0 = Nothing
    | i > 9 = Nothing
    | otherwise =
        if [c] == show i then Just i else Nothing

digit1, digit2 :: Maybe Int
digit1 = digit 0 "1n2345"
digit2 = digit 1 "1n2345"

binChar :: String -> Maybe Int
binChar s = digit 0 s <|> digit 1 s

digit3 :: Maybe Int
digit3 = binChar "1n2345"

class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a
    msum :: (Foldable t) => t (m a) -> m a
    msum = foldr mplus mzero

instance MonadPlus Maybe where
    mzero :: Maybe a
    mzero               = Nothing
    mplus :: Maybe a -> Maybe a -> Maybe a
    Nothing `mplus` Nothing = Nothing 
    Just x  `mplus` Nothing = Just x  
    Nothing `mplus` Just x  = Just x  
    Just x  `mplus` Just y  = Just x  

instance MonadPlus [] where
    mzero :: [a]
    mzero = [] 
    mplus :: [a] -> [a] -> [a] 
    mplus = (++) 

binCharM :: String -> Maybe Int
binCharM s = digit 0 s `mplus` digit 1 s

digit3M :: Maybe Int
digit3M = binChar "1n2345"

pythags :: [(Integer, Integer, Integer)]
pythags = [ (x, y, z) | z <- [1..], x <- [1..z], y <- [x..z], x^2 + y^2 == z^2 ]

pythagsM :: [(Integer, Integer, Integer)]
pythagsM = do
    z <- [1..100]
    x <- [1..z]
    y <- [x..z]
    guardM $ x^2 + y^2 == z^2
    return (x, y, z)
    where 
        guardM :: Bool -> [()]
        guardM True  = [()]
        guardM False = []

char :: Char -> String -> Maybe (Char, String)
char c s = do
    c' : s' <- return s
    guardM $ c == c'
    return (c, s')
    where 
        guardM :: Bool -> Maybe ()
        guardM True = Just ()
        guardM False = Nothing

class Monoid m where 
    mempty  :: m
    mappend :: m -> m -> m

instance Monoid [a] where
    mempty :: [a]
    mempty  = []
    mappend :: [a] -> [a] -> [a]
    mappend = (++)

