{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Used otherwise as a pattern" #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# HLINT ignore "Use tuple-section" #-}
module WikiBooks.Ch04 where

{-  
    @About
        functor (<$>), applicative functor (<*>), and (*>)
    @See
        Haskell/Prologue: IO, an applicative functor
        https://en.m.wikibooks.org/wiki/Haskell/Prologue:_IO,_an_applicative_functor
-}
import Text.Read
import Control.Monad
import Data.Maybe (fromMaybe)
import Data.Char

interactiveDoubling1 :: IO ()
interactiveDoubling1 = do
    putStrLn "Choose a number:"
    s <- getLine
    let ms = readMaybe s :: Maybe Double
    case ms of
        Just x ->
            putStrLn $
                "The double of your number is "
                ++ show (2*x)
        Nothing ->
            do
                putStrLn "This is not a valid number. Retrying..."
                interactiveDoubling1

interactiveDoubling2 :: IO ()
interactiveDoubling2 = do
    putStrLn "Choose a number:"
    -- ms :: Maybe Double  <- readMaybe <$> getLine
    ms <- readMaybe <$> getLine
    let
        mx :: Maybe Double
        mx = fmap (2 *) ms
    case mx of
        Just x -> do
            putStrLn $
                "The double of your number is "
                ++ show x
        Nothing -> do
            putStrLn
                "This is not a valid number. Retrying..."
            interactiveDoubling2


interactiveDoubling3 :: IO ()
interactiveDoubling3 = do
    ms <-
        putStrLn
            "Choose a number:" *>
        (readMaybe <$> getLine)
    let
        mx :: Maybe Double
        mx = fmap (2 *) ms
    case mx of
        Just x -> do
            putStrLn $
                "The double of your number is "
                ++ show x
        Nothing -> do
            putStrLn
                "This is not a valid number. Retrying..." *>
                interactiveDoubling2



interactiveSumming1 :: IO ()
interactiveSumming1 = do
    putStrLn "Choose two numbers:"
    sx <- getLine
    sy <- getLine
    let mx = readMaybe sx :: Maybe Double
        my = readMaybe sy
    case mx of
        Just x -> case my of
            Just y ->
                putStrLn $
                    "The sum of your numbers is "
                    ++ show (x+y)
            Nothing ->
                retry
        Nothing ->
            retry
    where
        retry :: IO ()
        retry = do
            putStrLn "Invalid number. Retrying..."
            interactiveSumming1

interactiveSumming2 :: IO ()
interactiveSumming2 = do
    putStrLn "Choose two numbers:"
    -- mx :: Maybe Double <- readMaybe <$> getLine
    -- my :: Maybe Double <- readMaybe <$> getLine
    mx <- readMaybe <$> getLine
    my <- readMaybe <$> getLine
    let
        msum :: Maybe Double
        msum = (+) <$> mx <*> my
    case msum of
        Just sum -> do
            putStrLn $
                "The sum of your numbers is "
                ++ show sum
        Nothing -> do
            putStrLn
                "Invalid number. Retrying..."
            interactiveSumming2

interactiveSumming3 :: IO ()
interactiveSumming3 = do
    (mx, my) <-
            putStrLn
                "Choose two numbers:" *>
                ((,) <$>
                    (readMaybe <$> getLine) <*>
                    (readMaybe <$> getLine))
    let
        msum :: Maybe Double
        msum = (+) <$> mx <*> my
    case msum of
        Just sum -> do
            putStrLn $
                "The sum of your numbers is "
                ++ show sum
        Nothing -> do
            putStrLn
                "Invalid number. Retrying..." *>
                interactiveSumming2

interactiveConcatenating1 :: IO ()
interactiveConcatenating1 = do
    putStrLn "Choose two strings:"
    sx <- getLine
    sy <- getLine
    putStrLn "Let's concatenate them:"
    putStrLn (sx ++ sy)

interactiveConcatenating2 :: IO ()
interactiveConcatenating2 = do
    putStrLn
        "Choose two strings:"
    ss <- (++) <$> getLine <*> getLine
    putStrLn
        "Let's concatenate them:"
    putStrLn
        ss

interactiveConcatenating3 :: IO ()
interactiveConcatenating3 = do
    ss <-
        putStrLn
            "Choose two strings:" *>
            ((++) <$> getLine <*> getLine)
    putStrLn
        "Let's concatenate them:" *>
        putStrLn
            ss

{-  
    @About
        Basic Monad
    @See
        Haskell/Understanding monads
        https://en.m.wikibooks.org/wiki/Haskell/Understanding_monads
-}
data Person =
    A | B | C | D | E | F | G | H
    deriving (Show)
{-
    E - F   G - H
     \ /     \ /
      C   -   D
     /         \
    A           B
-}

father :: Person -> Maybe Person
father A = Just C
father B = Just C
father C = Just E
father D = Just G
father _ = Nothing

mother :: Person -> Maybe Person
mother A = Just D
mother B = Just D
mother C = Just F
mother D = Just H
mother _ = Nothing

mgrandfather1 :: Person -> Maybe Person
mgrandfather1 p =
    case mother p of
        Just mom ->
            father mom
        Nothing ->
            Nothing

mgrandfather2 :: Person -> Maybe Person
mgrandfather2 p =
    mother p >>= father
    -- return p >>= mother >>= father

mgrandfather3 :: Person -> Maybe Person
mgrandfather3 p = do
    pmom <- mother p
    father pmom

bothGrandfathers1 :: Person -> Maybe (Person, Person)
bothGrandfathers1 p =
    case father p of
        Nothing -> Nothing
        Just dad ->
            case father dad of
                Nothing -> Nothing
                Just gf1 ->                          -- found first grandfather
                    case mother p of
                        Nothing -> Nothing
                        Just mom ->
                            case father mom of
                                Nothing -> Nothing
                                Just gf2 ->          -- found second grandfather
                                    Just (gf1, gf2)

bothGrandfathers2 :: Person -> Maybe (Person, Person)
bothGrandfathers2 p =
    case father p >>= father of
        Just fgf ->
            case mother p >>= father of
                Just mgf ->
                    Just (fgf, mgf)
                Nothing ->
                    Nothing
        Nothing ->
            Nothing

bothGrandfathers3 :: Person -> Maybe (Person, Person)
bothGrandfathers3 p =
    case (father p >>= father, mother p >>= father) of
        (Just fgf, Just mgf) ->
            Just (fgf, mgf)
        _ ->
            Nothing

bothGrandfathers4 :: Person -> Maybe (Person, Person)
bothGrandfathers4 p = do
    pf <- father p
    pm <- mother p
    pfgf <- father pf
    pmgf <- father pm
    return (pfgf, pmgf)

bothGrandfathers5 :: Person -> Maybe (Person, Person)
bothGrandfathers5 p = do
    pfgf <- father p >>= father
    pmgf <- mother p >>= father
    return (pfgf, pmgf)

bothGrandfathers6 :: Person -> Maybe (Person, Person)
bothGrandfathers6 p = do
    (,) <$>
            (father p >>= father) <*>
            (mother p >>= father)


data Maybe' a =
    Nothing'
    | Just' a
    deriving (Show)

instance Monad Maybe' where
    return :: a -> Maybe' a
    return = Just'
    (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
    (>>=) Nothing' _ = Nothing'
    (>>=) (Just' x) f = f x

instance Applicative Maybe' where
    pure :: a -> Maybe' a
    pure = return
    (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
    (<*>) = ap

instance Functor Maybe' where
     fmap :: (a -> b) -> Maybe' a -> Maybe' b
     fmap = liftM

{-
    @About
        Maybe monad
    @See
        Haskell/Understanding monads/Maybe
        https://en.m.wikibooks.org/wiki/Haskell/Understanding_monads/Maybe
-}

-- Example 1: safe function
safeLog :: (Floating a, Ord a) => a -> Maybe a
safeLog x
    | x > 0     = Just . log $ x
    | otherwise = Nothing

safeSqrt :: (Floating a, Ord a) => a -> Maybe a
safeSqrt x
    | x > 0 = Just . sqrt $ x
    | otherwise = Nothing

badsqrtlog :: (Floating a, Ord a) => a -> a
badsqrtlog = sqrt . log

goodsqrtlog1 :: (Floating a, Ord a) => a -> Maybe a
goodsqrtlog1 x = safeLog x >>= safeSqrt

goodsqrtlog2 :: (Floating a, Ord a) => a -> Maybe a
goodsqrtlog2 x = do
    y <- safeLog x
    safeSqrt y

goodsqrtlog3 :: (Floating a, Ord a) => a -> Maybe a
goodsqrtlog3 = safeSqrt <=< safeLog

-- Example 2: lookup table
type Name = String
type PhoneNumber = String
type RegistrationNumber = String
type Tax = Double

phonebook :: [(Name, PhoneNumber)]
phonebook = [ ("Bob",   "01788 665242"),
              ("Fred",  "01624 556442"),
              ("Alice", "01889 985333"),
              ("Jane",  "01732 187565") ]
governmentDatabase :: [(PhoneNumber, RegistrationNumber)]
governmentDatabase = [ ("01788 665242", "0001"),
              ("01624 556442","0002"),
              ("01889 985333","0003"),
              ("01732 187565","0004") ]
taxDatabase :: [(RegistrationNumber, Tax)]
taxDatabase = [ ("0001", 15432),
              ("0002", 154321),
              ("0003", 16664),
              ("0004", 12) ]

badlookup :: (Eq a) => a -> [(a, b)] -> b
badlookup _ [] = error "No information!"
badlookup k ((x,y):rest)
    | k == x = y
    | otherwise =
        badlookup k rest

goodlookup :: (Eq a) => a -> [(a, b)] -> Maybe b
goodlookup _ [] = Nothing
goodlookup k ((x,y):rest)
    | k == x = Just y
    | otherwise = goodlookup k rest

-- Name -> Maybe PhoneNumber                    // a -> m b
-- PhoneNumber -> Maybe RegistrationNumber      // b -> m c
-- RegistrationNumber -> Maybe Tax              // c -> m d
getRegistrationNumber1 :: Name -> Maybe RegistrationNumber
getRegistrationNumber1 name =
    goodlookup name phonebook >>=
        (\phoneNumber -> goodlookup phoneNumber governmentDatabase)
-- (a -> m b) -> (b -> m c) -> (a -> m c)
getRegistrationNumber2 :: Name -> Maybe RegistrationNumber
getRegistrationNumber2 =
    (\name -> goodlookup name phonebook) >=>
        (\phoneNumber -> goodlookup phoneNumber governmentDatabase)
getRegistrationNumber3 :: Name -> Maybe RegistrationNumber
getRegistrationNumber3 name = do
    phoneNumber <- goodlookup name phonebook
    goodlookup phoneNumber governmentDatabase

getTaxOwed1 :: Name -> Maybe Tax
getTaxOwed1 name =
    goodlookup name phonebook >>=
        (\phoneNumber -> goodlookup phoneNumber governmentDatabase) >>=
            (\registrationNumber -> goodlookup registrationNumber taxDatabase)
-- (a -> m b) -> (b -> m c) -> (c -> m d) -> (a -> m d)
getTaxOwed2 :: Name -> Maybe Tax
getTaxOwed2 =
    (\name -> goodlookup name phonebook) >=>
        (\phoneNumber -> goodlookup phoneNumber governmentDatabase) >=>
            (\registrationNumber -> goodlookup registrationNumber taxDatabase)
getTaxOwed3 :: Name -> Maybe Tax
getTaxOwed3 name = do
    phoneNumber <- goodlookup name phonebook
    registrationNumber <- goodlookup phoneNumber governmentDatabase
    goodlookup registrationNumber taxDatabase

-- Example 3: extracting values // comonad?
zeroAsDefault1 :: Maybe Int -> Int
zeroAsDefault1 mx =
    case mx of
        Nothing -> 0
        Just x -> x

zeroAsDefault2 :: Maybe Int -> Int
zeroAsDefault2 = fromMaybe 0

displayResult1 :: Maybe Int -> String
displayResult1 =
    maybe "There was no result" (\number -> "The result was " ++ show number)

{-
    @About
        List monad
    @See
        Haskell/Understanding monads/List
        https://en.m.wikibooks.org/wiki/Haskell/Understanding_monads/List
-}

{- Same functor and monad but different applicative -}
infixr 5 :-
data List a =
    Nil
    | a :- (List a)

instance (Show a) => Show (List a) where
    show :: List a -> String
    show Nil = []
    show (x:-xs) = show (x:isomorphism xs)
        where
            isomorphism :: List a -> [a]
            isomorphism Nil = []
            isomorphism (x:-xs) = x: isomorphism xs

    -- show (x:-xs) = [show x ++ show xs]

instance Semigroup (List a) where
    (<>) :: List a -> List a -> List a
    (<>) Nil ys = ys
    (<>) (x:-xs) ys = x:-(xs<>ys)
instance Monoid (List a) where
    mempty :: List a
    mempty = Nil
instance Foldable List where
    foldMap :: (Monoid m) => (a -> m) -> List a -> m
    foldMap _ Nil = mempty
    foldMap f (x:-xs) = f x `mappend` foldMap f xs

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap _ Nil = Nil
    fmap f (x :- xs) = f x :- fmap f xs
instance Applicative List where
    pure :: a -> List a
    pure x = x :- Nil
    (<*>) :: List (a -> b) -> List a -> List b
    (<*>) _ Nil = Nil
    (<*>) (f:-fs) (x:-xs) = f x:-(fs<*>xs)
instance Monad List where
    (>>=) :: List a -> (a -> List b) -> List b
    xs >>= f = foldMap (<>) (fmap f xs) Nil
        -- foldMap f xs
    -- Nil >>= _ = Nil
    -- (x:-xs) >>= f = f x <> (xs >>= f)


-- Example 1: Bunny invasion
generation :: String -> [String]
generation = replicate 3
generation' :: String -> List String
generation' li = return li <> return li <> return li

bunny, br :: [String]
bunny = ["bunny"]
br = ["bunny", "rabbit"]

bunny', br' :: List String
br' = "bunny" :- "rabbit" :- Nil
bunny' = "buuny" :- Nil

gen0 :: [String] -> [String]
gen0 str = str >>= generation
gen0' :: List String -> List String
gen0' str = str >>= generation'

gen1 :: [String] -> [String]
gen1 str = str >>= generation >>= generation
gen1' :: List String -> List String
gen1' str = str >>= generation' >>= generation'

gen2 :: [String] -> [String]
gen2 str = do
    x <- str
    y <- generation x
    generation y

gen2' :: List String -> List String
gen2' str = do
    x <- str
    y <- generation' x
    generation' y

-- Exercise: Implement themselvesTimes :: [Int] -> [Int], which takes each number 
-- in the argument list and generates copies of it in the result list.

themselvesTimes1 :: [Int] -> [Int]
themselvesTimes1 [] = []
themselvesTimes1 (n:ns) = replicate n n <> themselvesTimes1 ns
themselvesTimes1' :: List Int -> List Int
themselvesTimes1' Nil = Nil
themselvesTimes1' (n:-ns) = replicateL n n <> themselvesTimes1' ns

themselvesTimes2 :: [Int] -> [Int]
themselvesTimes2 xs = foldl (.) id ((\z w -> replicate z z <> w)<$> xs) []
themselvesTimes2' :: List Int -> List Int
themselvesTimes2' xs = foldl (.) id ((\z w -> replicateL z z <> w) <$> xs) Nil

themselvesTimes3 :: [Int] -> [Int]
themselvesTimes3 xs = do
    x <- xs
    replicate x x
themselvesTimes3' :: List Int -> List Int
themselvesTimes3' xs = do
    x <- xs
    replicateL x x

themselvesTimes4 :: [Int] -> [Int]
themselvesTimes4 xs = xs >>= (\x -> replicate x x)
themselvesTimes4' :: List Int -> List Int
themselvesTimes4' xs = xs >>= (\x -> replicateL x x)

themselvesTimes9 :: [Int] -> [Int]
themselvesTimes9 ns = foldl (<>) [] $ replicate <$> ns <*> ns
themselvesTimes9' :: List Int -> List Int
themselvesTimes9' ns = foldl (<>) Nil $ replicateL <$> ns <*> ns

replicateL :: Int -> a -> List a
replicateL n x
    | n <= 1  = x:-Nil
    | otherwise = x :- replicateL (n-1) x

-- Example 2: Board game 
type SimpleBoard = Int

-- position x on the board only can move +2 or -1
nextConfigs :: SimpleBoard -> [SimpleBoard]
nextConfigs position = [position+2, position-1]

onelater :: SimpleBoard -> [SimpleBoard]
onelater = nextConfigs
twolater :: SimpleBoard -> [SimpleBoard]
twolater = nextConfigs >=> nextConfigs
threelater :: SimpleBoard -> [SimpleBoard]
threelater = nextConfigs >=> nextConfigs >=> nextConfigs
twolater2 :: SimpleBoard -> [SimpleBoard]
twolater2 position = do
    onelater <- nextConfigs position
    nextConfigs onelater
threelater2 :: SimpleBoard -> [SimpleBoard]
threelater2 position = do
    onelater <- nextConfigs position
    twolater <- nextConfigs onelater
    nextConfigs twolater

nlater :: Int -> SimpleBoard -> [SimpleBoard]
nlater n position
    | n <= 0 = return position
    | otherwise = nlater (n-1) position >>= nextConfigs

-- Effective computation 1: slower than using Data.Set (fromList, toList)
removeDuplicate :: Eq a => [a] -> [a]
removeDuplicate [] = []
removeDuplicate (x:xs)
    | x `elem` xs = removeDuplicate xs
    | otherwise = x: removeDuplicate xs

nlaterE :: Int -> SimpleBoard -> [SimpleBoard]
nlaterE n position
    | n <= 0 = return position
    | otherwise = removeDuplicate $ nlaterE (n-1) position >>= nextConfigs

-- Effective computation 2: slower than Ecomputation 1
class Monad' m where
    return' :: a -> m a
    (>>>=) :: (Eq b) => m a -> (a -> m b) -> m b
class ListLike l where
    liketoList :: l a -> [a]
    likefromList :: [a] -> l a

infixr 5 :=
data MySet a =
    EmptySet
    | a := MySet a
    deriving (Eq)
instance ListLike MySet where
    liketoList :: MySet a -> [a]
    liketoList EmptySet = []
    liketoList (x:=xs) = x:liketoList xs
    likefromList :: [a] -> MySet a
    likefromList [] = EmptySet
    likefromList (x:xs) = x:=likefromList xs

instance (Show a) => Show (MySet a) where
    show :: MySet a -> String
    show = show . liketoList

instance (Eq a) => Semigroup (MySet a) where
    (<>) :: MySet a -> MySet a -> MySet a
    EmptySet <> ys = ys
    (x:=xs) <> ys
        | x `elem` ys = xs<>ys
        | otherwise = x:=(xs<>ys)
instance (Eq a) => Monoid (MySet a) where
    mempty :: MySet a
    mempty = EmptySet

instance Foldable MySet where
    foldMap :: (Monoid m) => (a -> m) -> MySet a -> m
    foldMap _ EmptySet = mempty
    foldMap f (x:=xs) = f x <> foldMap f xs

instance Monad' MySet where
    return' :: a -> MySet a
    return' x = x := EmptySet
    (>>>=) :: (Eq b) => MySet a -> (a -> MySet b) -> MySet b
    (>>>=) EmptySet _ = EmptySet
    (>>>=) xs f = foldMap f xs

nextConfigsEE :: SimpleBoard -> MySet SimpleBoard
nextConfigsEE = likefromList . nextConfigs

nlaterEE :: Int -> SimpleBoard -> MySet SimpleBoard
nlaterEE n position
    | n <= 0 = return' position
    | otherwise = nlaterEE (n-1) position >>>= nextConfigsEE

{-
    @About
        Do notation
    @See
        Haskell/do notation
        https://en.m.wikibooks.org/wiki/Haskell/do_notation
-}
-- Example: user-interactive program
nameDo1 :: IO ()
nameDo1 = do
    putStr "What is your first name?"
    first <- getLine
    putStr "And your last name?"
    last <- getLine
    let full = first ++ " " ++ last
    putStrLn $ "Pleased to meet you, " ++ full ++ "!"

nameDo2 :: IO ()
nameDo2 = do
    first <-
        putStr "What is your first name?" >> getLine
    last <-
        putStr "And your last name?" >> getLine
    putStrLn $
        "Pleased to meet you, " ++ first ++ " " ++ last ++ "!"

nameDo3 :: IO ()
nameDo3 =
    putStr "What is your first name?" >>
        getLine >>=
            (\first ->
                putStr "And your last name?"  >>
            getLine >>=
                (\last ->
                    putStrLn $
                        "Pleased to meet you, " ++
                        first ++
                        " " ++
                        last ++
                        "!"
                ))

-- Example: Returning values
nameReturn :: IO String
nameReturn = do
    first <-
        putStr "What is your first name?" >> getLine
    last <-
        putStr "And your last name?" >> getLine
    let
        full = first ++ " " ++ last
    putStrLn $
        "Pleased to meet you, " ++ full ++ "!"
    return full

greetAndSeeYou :: IO ()
greetAndSeeYou = do
    name <- nameReturn
    putStrLn $
        "See you, " ++
        name ++
        "!"

-- Example: Just sugar
greetAndSeeYou2 :: IO ()
greetAndSeeYou2 =
    -- IO String -> (String -> IO ()) -> IO ()
    nameReturn >>=  putStrLn . seeYou
    where
        seeYou :: String -> String
        seeYou name = "See you, " ++ name ++ "!"

greetAndSeeYou3 :: IO ()
greetAndSeeYou3 =
    fmap seeYou nameReturn >>= putStrLn
    where
        seeYou :: String -> String
        seeYou name = "See you, " ++ name ++ "!"

{-
    @About
        Haskell/Understanding monads/IO
    @See
        https://en.m.wikibooks.org/wiki/Haskell/Understanding_monads/IO
-}
-- Example: Combining functions and I/O actions
echo1 :: IO ()
echo1 =
    -- (>>) :: IO () -> IO [Char] -> IO ()
    putStrLn "Write your string: " >>
        -- fmap :: ([Char] -> [Char]) -> IO [Char] -> IO [Char]
        fmap shout getLine
            -- (>>=) :: IO [Char] -> ([Char] -> IO ()) -> IO ()
            >>= putStrLn
            where
                shout :: [Char] -> [Char]
                shout = map toUpper

echo2 :: IO ()
echo2 = do
    str <-
        putStrLn "Write your string: " >>
            getLine
    putStrLn $
        fmap toUpper str

-- Example: Pure and impure
-- impure version
speakTo :: (String -> String) -> IO String
speakTo fSentence =
    fmap fSentence getLine

sayHello :: IO String
sayHello =
    speakTo $
        \name -> "Hello, " ++ name ++ "!"

-- pure version
sayHello2 :: IO String
sayHello2 =
    fmap (\name -> "Hello, " ++ name ++ "!") getLine

sayHello3 :: IO String
sayHello3 = do
    name <- getLine
    return $
        "Hello, " ++ name ++ "!"

sayHello4 :: IO String
sayHello4 = getLine >>=
    (\name ->
        return $ "Hello, " ++ name ++ "!")

-- Example: Functional and imperative
readln1 :: IO ()
readln1 = do
    x :: Int <- readLn
    print x

-- readln2 :: IO ()
-- readln2 = 
--     readLn >>= 
--         (\x -> 
--             print x)

-- Example: Monadic control structures
-- fiveGetLines :: [IO String]
fiveGetLines1 :: IO [String]
fiveGetLines1 =
    -- sequence :: [IO String] -> IO [String]
    sequence $
        -- replicate :: Int -> (IO String) -> [IO String]
        replicate 5 getLine

fiveGetLines2 :: IO [String]
fiveGetLines2 =
    -- replicateM :: Int -> (IO String) -> IO [String]
    replicateM 5 getLine

fiveGetLine3 :: IO [String]
fiveGetLine3 =
    -- mapM :: (a -> IO String) -> [a] -> IO [String]
    mapM (const getLine) [1..5]


fiveGetLines9 :: IO [String]
fiveGetLines9 = do
    (\x y z p q -> x : y : z : p : [q]) <$>
        getLine <*>
        getLine <*>
        getLine <*>
        getLine <*>
        getLine

-- Exercise1 : What is the expected behavior of sequence for the Maybe monad?
mSquence1 :: [a] -> Maybe [a]
mSquence1 list =
    sequence $ fmap Just list

mSquence2 :: [a] -> Maybe [a]
mSquence2 list =
    sequence $ (fmap Just list ++ [Nothing])

{-
    @About
        Haskell/Understanding monads/State
    @See
        https://en.m.wikibooks.org/wiki/Haskell/Understanding_monads/State
-}

data TurnstileState =
    Locked
    | Unlocked
    deriving (Eq, Show)

data TurnstileOutput =
    Thank
    | Open
    | Tut
    deriving (Eq, Show)

coin, push :: TurnstileState -> (TurnstileOutput, TurnstileState)
coin _          = (Thank,Unlocked)
push Unlocked   = (Open,Locked)
push Locked     = (Tut,Locked)

monday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
monday s0 =
    let
        (a1, s1) = coin s0
        (a2, s2) = push s1
        (a3, s3) = push s2
        (a4, s4) = coin s3
        (a5, s5) = push s4
    in
        ([a1, a2, a3, a4, a5], s5)
monday2 :: TurnstileState -> TurnstileState
monday2 s0 =
    let
        (_, s1) = coin s0
        (_, s2) = push s1
        (_, s3) = push s2
        (_, s4) = coin s3
        (_, s5) = push s4
    in
        s5

-- Exericses
regularPerson, distractedPerson, hastyPerson :: TurnstileState ->
        ([TurnstileOutput], TurnstileState)
regularPerson s0 =
    let
        (a1, s1) = coin s0
        (a2, s2) = push s1
    in
        ([a1,a2], s2)
distractedPerson s0 =
    let
        (a1, s1) = coin s0
    in
        ([a1], s1)
hastyPerson s0 =
    let
        (a1, s1) = push s0
    in
        case a1 of
            Open -> ([a1], s1)
            Tut ->
                let
                    (a2, s2) = coin s1
                    (a3, s3) = push s2
                in
                    ([a1,a2,a3], s3)

tuesday :: TurnstileState -> ([TurnstileOutput], TurnstileState)
tuesday s0 =
    let
        (a1, s1) = regularPerson s0
        (a2, s2) = hastyPerson s1
        (a3, s3) = distractedPerson s2
        (a4, s4) = hastyPerson s3
    in
        (mconcat [a1,a2,a3,a4], s4)
        -- (a1++a2++a3++a4, s4)

luckyPair :: Bool -> TurnstileState -> (Bool, TurnstileState)
luckyPair bool s0 =
    let
        (a1, s1) = if bool then regularPerson s0 else distractedPerson s0
        (a2, s2) = push s1
    in
        (a2 == Open, s2)

class StateLike m where
    fromS :: (s -> (a, s)) -> m s a
    toS :: m s a -> (s -> (a, s))
    joinS :: m s (m s a) -> m s a
    evalS :: m s a -> s -> a
    evalS p = fst . toS p
    execS :: m s a -> s -> s
    execS p = snd . toS p
    modS :: (s -> s) -> m s ()
    modS f = fromS $ \s -> ((), f s)
    putS :: s -> m s ()
    putS s = modS $ \_ -> s
    -- putS s = fromS $ \_ -> ((), s)
    getsS :: (s -> a) -> m s a
    getsS f = fromS $ \s -> (f s, s)
    getS :: m s s
    getS = getsS $ \s -> s
    -- getS = fromS $ \s -> (s, s)

newtype MyState s a = MyState { runMyState :: s -> (a, s) }
instance StateLike MyState where
    fromS :: (s -> (a, s)) -> MyState s a
    fromS = MyState
    toS :: MyState s a -> s -> (a, s)
    toS = runMyState
    joinS :: MyState s (MyState s a) -> MyState s a
    joinS h =
        fromS $
            \s0 ->
                let
                    (g, s1) = toS h s0
                    (a, s2) = toS g s1
                in
                    (a, s2)

instance Functor (MyState s) where
    fmap :: (a -> b) -> MyState s a -> MyState s b
    fmap f h = fromS $
        \s0 ->
            let
                (a1, s1) = toS h s0
            in
                (f a1, s1)

instance Applicative (MyState s) where
    pure :: a -> MyState s a
    pure a = fromS $ \s -> (a, s)
    -- (<*>) :: (s -> ((a -> b), s)) -> (s -> (a, s)) -> (s -> (b, s))
    (<*>) :: MyState s (a -> b) -> MyState s a -> MyState s b
    (<*>) f h =
        fromS $
            \s0 ->
                let
                    (g, s1) = toS f s0
                    (a, s2) = toS h s1
                in
                    (g a, s2)

instance Monad (MyState s) where
    (>>=) :: MyState s a -> (a -> MyState s b) -> MyState s b
    -- f >>= h = joinS . fmap h $ f
    f >>= h =
        fromS $
            \s0 ->
                let
                    (a1, s1) = toS f s0
                in
                    toS (h a1) s1

coinS, pushS :: MyState TurnstileState TurnstileOutput
coinS = do
    putS Unlocked
    return Thank
pushS = do
    s <- getS
    putS Locked
    case s of
        Locked   -> return Tut
        Unlocked -> return Open

mondayS :: MyState TurnstileState [TurnstileOutput]
mondayS = do
    a1 <- coinS
    a2 <- pushS
    a3 <- pushS
    a4 <- coinS
    a5 <- pushS
    return [a1, a2, a3, a4, a5]
mondayS' :: MyState TurnstileState [TurnstileOutput]
mondayS' = coinS >>=
    (\a1 -> pushS >>=
        (\a2 -> pushS >>=
            (\a3 -> coinS >>=
                (\a4 -> pushS >>=
                    (\a5 ->
                        return [a1, a2, a3, a4, a5])))))

mondayS2 :: MyState TurnstileState TurnstileOutput
mondayS2 = coinS >> pushS >> pushS >> coinS >> pushS

-- Exercises
regularPersonS, distractedPersonS :: MyState TurnstileState [TurnstileOutput]
regularPersonS = sequence [coinS, pushS]
distractedPersonS = sequence [coinS]
-- regularPersonS = do
--     a1 <- coinS
--     a2 <- pushS
--     return [a1,a2]
-- distractedPersonS = do
--     a1 <- coinS
--     return [a1]
hastyPersonS :: MyState TurnstileState [TurnstileOutput]
hastyPersonS = do
    a1 <- pushS
    if a1 == Open
        then return [a1]
        else
            do
                a2 <- coinS
                a3 <- pushS
                return [a1,a2,a3]

luckyPairS :: Bool -> MyState TurnstileState Bool
luckyPairS bool = do
    a1 <- if bool then regularPersonS else distractedPersonS
    a2 <- pushS
    return $ a2 == Open

testTurnstile :: MyState TurnstileState Bool
testTurnstile = do
    --somehow set state to Locked
    putS Locked --   fromS $ \_ -> ((),Locked)
    check1 <- pushS
    --somehow set state to Unlocked
    putS Unlocked --   fromS $ \_ -> ((),Unlocked)
    check2 <- pushS
    --somehow set state to Locked again
    putS Locked --   fromS $ \_ -> ((),Locked)
    return (check1 == Tut && check2 == Open)

test1 :: [TurnstileOutput]
test1 = evalS (replicateM 6 pushS) Unlocked

data TurnstileInput = Coin | Push
  deriving (Eq, Show)
  
turnS :: TurnstileInput -> MyState TurnstileState TurnstileOutput
turnS = fromS . turn 
    where
        turn :: TurnstileInput -> TurnstileState -> (TurnstileOutput, TurnstileState)
        turn Coin _        = (Thank, Unlocked)
        turn Push Unlocked = (Open,  Locked)
        turn Push Locked   = (Tut,   Locked)

test2 :: (TurnstileOutput, TurnstileState)
test2 = toS (turnS Coin) Locked

test3 :: [TurnstileOutput]
test3 = evalS (mapM turnS [Coin, Push, Push, Coin, Push]) Locked
test1' :: [TurnstileOutput]
test1' = evalS (mapM turnS [Push, Push, Push, Push, Push, Push]) Unlocked

getsThroughS :: TurnstileInput -> MyState TurnstileState Bool
getsThroughS input = do
  output <- turnS input
  return $ output == Open

test4, test5 :: [TurnstileInput]
test4 = evalS (filterM getsThroughS [Push, Coin, Coin, Push, Push, Coin, Push]) Locked
test5 = evalS (filterM getsThroughS [Coin, Push, Coin, Push, Push, Coin, Push]) Locked

countOpens :: [TurnstileInput] -> MyState TurnstileState Int
countOpens = foldM incIfOpens 0 
    where
        incIfOpens :: Int -> TurnstileInput -> MyState TurnstileState Int
        incIfOpens n i = do
            g <- getsThroughS i
            if g then return (n+1) else return n

test6 :: Int
test6 = evalS (countOpens [Coin, Push, Coin, Push, Push, Coin, Push]) Locked

regularPersonS', distractedPersonS' :: MyState TurnstileState [TurnstileOutput]
regularPersonS' = mapM turnS [Coin, Push]
distractedPersonS' = mapM turnS [Coin]
-- hastyPersonS' :: MyState TurnstileState [TurnstileOutput]
-- hastyPersonS' = mapM turnS [Push, Coin, Push]

tuesdayS :: MyState TurnstileState [TurnstileOutput]
tuesdayS = do
    a1 <- regularPersonS
    a2 <- hastyPersonS
    a3 <- distractedPersonS
    a4 <- hastyPersonS
    return $ mconcat [a1,a2,a3,a4]

saveCoinsS :: [TurnstileInput] -> MyState TurnstileState Int
saveCoinsS = foldM saveCoins 0 
    where
        saveCoins :: Int -> TurnstileInput -> MyState TurnstileState Int
        saveCoins n input = do 
            output <- turnS input
            case output of
                Thank -> return $ n + 1
                Open -> return $ n - 1
                _ -> return n
        --     g1 <- coinPlusS input
        --     g2 <- coinMinusS input
        --     if g1 
        --         then return $ n+1 
        --     else if g2 
        --         then return $ n-1
        --     else return n
        -- coinPlusS :: TurnstileInput -> MyState TurnstileState Bool
        -- coinPlusS input = do
        --     output <- turnS input
        --     return $ output == Thank
        -- coinMinusS :: TurnstileInput -> MyState TurnstileState Bool
        -- coinMinusS input = do
        --     output <- turnS input
        --     return $ output == Open

test7, test7', test7'' :: Int
test7 = evalS (saveCoinsS [Push, Coin, Coin, Coin, Push, Push, Coin, Push]) Locked
test7' = evalS (saveCoinsS [Coin, Push, Coin, Push, Coin, Push, Push]) Locked
test7'' = evalS (saveCoinsS [Coin, Push, Coin, Coin]) Locked

-- sequenceUntil :: (a -> Bool) -> [MyState s a] -> MyState s [a]
-- sequenceUntil f xs = filterM f $ sequence xs
-- sequenceUntil f xs = sequence . filterM (\z -> return $ f z) xs
-- sequenceUntil f = sequence 
