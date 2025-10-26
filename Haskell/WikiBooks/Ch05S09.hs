{-
    @About
        Haskell/Monad transformers
    @See
        https://en.m.wikibooks.org/wiki/Haskell/Monad_transformers
-}
module WikiBooks.Ch05S09 where

import Data.Char ( isAlpha, isPunctuation, isNumber )
import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (StateT, runStateT)
import Control.Monad.Trans.Class

getPassphrase :: IO (Maybe String)
getPassphrase = do
    s <- getLine
    if isValid s
        then return $ Just s
        else return Nothing

-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

askPassphrase :: IO ()
askPassphrase = do
    putStrLn "Insert your new passphrase:"
    maybe_value <- getPassphrase
    case maybe_value of
        Just value -> do putStrLn "Storing in database..."  -- do stuff
        Nothing -> putStrLn "Passphrase invalid."

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
instance Monad m => Functor (MaybeT m) where
    fmap :: (a -> b) -> MaybeT m a -> MaybeT m b
    fmap f maybetma = MaybeT $ do
            maybea <- runMaybeT maybetma
            return $ f <$> maybea
    -- fmap f maybetma = MaybeT $ 
    --     fmap (fmap f) (runMaybeT maybetma)
instance Monad m => Applicative (MaybeT m) where
    pure :: a -> MaybeT m a
    pure  = MaybeT . return . Just
    (<*>) :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    maybetmf <*> maybetma = MaybeT $ do
        maybef <- runMaybeT maybetmf
        maybea <- runMaybeT maybetma
        return $ maybef <*> maybea
instance Monad m => Monad (MaybeT m) where
    (>>=) ::  MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    maybetma >>= maybetmf = MaybeT $
        let
            -- mmaybea :: m (Maybe a)
            mmaybea = runMaybeT maybetma
            -- mmaybef :: Maybe a -> m (Maybe b)
            mmaybef (Just z) = runMaybeT $ maybetmf z
            mmaybef Nothing = return Nothing
        in
            mmaybea >>= mmaybef

instance (Monad m) => Alternative (MaybeT m) where
    empty :: (Monad m) => MaybeT m a
    empty   = MaybeT $ return Nothing
    (<|>) :: (Monad m) => MaybeT m a -> MaybeT m a -> MaybeT m a
    x <|> y = MaybeT $ do
                maybex <- runMaybeT x
                case maybex of
                    Just _     -> return maybex
                    Nothing    -> runMaybeT y

instance (Monad m) => MonadPlus (MaybeT m) where
    mzero :: Monad m => MaybeT m a
    mzero = empty
    mplus :: Monad m => MaybeT m a -> MaybeT m a -> MaybeT m a
    mplus = (<|>)

instance MonadTrans MaybeT where
    lift :: Monad m => m a -> MaybeT m a
    -- lift = MaybeT . (liftM Just)
    -- lift = MaybeT . fmap Just
    lift ma = MaybeT $ do
        -- a :: a
        a <- ma
        -- return _ :: m (Maybe a)
        return $ Just a

    -- lift ma0 = MaybeT $ do 
    --     let 
    --         ma1 = do
    --             a <- ma0
    --             return $ Just a
    --     ma1

getPassphraseT :: MaybeT IO String
getPassphraseT = do
    s <- lift getLine
    guardT $ isValid s
    return s
    where
        guardT :: Bool -> MaybeT IO ()
        guardT True = return ()
        guardT False = mzero

askPassphraseT :: MaybeT IO ()
askPassphraseT = do
    lift $ putStrLn "Insert your new passphrase:"
    value <- getPassphraseT
    lift $ putStrLn "Storing in database..."

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    ida >>= f = f $ runIdentity ida

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f ida = Identity . f $ runIdentity ida

instance Applicative Identity where
    pure :: a -> Identity a
    pure = Identity
    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    idf <*> ida = Identity . runIdentity idf $ runIdentity ida


newtype IdentityT m a = IdentityT { runIdentityT :: m (Identity a)}

instance Monad m => Functor (IdentityT m) where
    fmap :: (a -> b) -> IdentityT m a -> IdentityT m b
    fmap f idtma = IdentityT $ do
        ida <- runIdentityT idtma
        return $ f <$> ida

instance Monad m => Applicative (IdentityT m) where
    pure :: Monad m => a -> IdentityT m a
    pure = IdentityT . return . Identity
    (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
    idtmf <*> idtma = IdentityT $ do
        midf <- runIdentityT idtmf
        mida <- runIdentityT idtma
        return $ midf <*> mida

instance Monad m => Monad (IdentityT m) where
    (>>=) :: Monad m => IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    idtma >>= f = IdentityT $ do
        let
            -- mida :: m Identity a
            mida = runIdentityT idtma
            -- idf :: Identity a -> m Identity b
            idf (Identity z) = runIdentityT $ f z
        -- m Identity b
        mida >>= idf

instance MonadTrans IdentityT where
    lift :: Monad m => m a -> IdentityT m a
    lift ma0 = IdentityT $ do
        -- a :: a 
        a <- ma0
        -- return ida :: m (Identity a)
        return $ Identity a

{-
    @About
        State transformer
-}

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
instance (Monad m) => Functor (StateT s m) where
    fmap :: (a -> b) -> StateT s m a -> StateT s m b
    fmap f stsma = StateT $ 
        \s0 -> do
            -- runStateT stsma s0 :: m (a,s)
            (a,s1) <- runStateT stsma s0
            -- (f a, s1) :: (b,s)
            return (f a, s1)
instance (Monad m) => Applicative (StateT s m) where
    pure :: a -> StateT s m a 
    pure a = StateT $ 
        \s0 -> 
            return (a,s0)
    (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
    stsmf <*> stsma = StateT $ 
        \s0 -> do
            -- runState stsmf s0 :: m (a -> b,s)
            (f, s1) <- runStateT stsmf s0
            -- runStateT stsma s1 :: m (a,s)
            (a, s2) <- runStateT stsma s1
            -- (f a, s2) :: (b,s)
            return (f a, s2)

instance (Monad m) => Monad (StateT s m) where
    (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    stsma >>= afstsmb = StateT $ 
        \s0 -> do 
            -- runStateT stsma s0 :: m (a,s)
            (a,s1) <- runStateT stsma s0
            -- runStateT (afstsmb a) s1 :: m (b,s)
            runStateT (afstsmb a) s1

instance MonadTrans (StateT s) where
    lift :: Monad m => m a -> StateT s m a
    lift ma = StateT $ 
        \s0 -> ma >>= (\z -> return (z, s0))
        -- \s0 -> do
        --     a <- ma
        --     return (a,s0)

instance Monad m => MonadState s (StateT s m) where
    get :: Monad m => StateT s m s
    get = StateT $ \s -> return (s,s)
    put :: Monad m => s -> StateT s m ()
    put s = StateT $ \_ -> return ((), s)

instance MonadPlus m => MonadPlus (StateT s m) where
    mzero :: MonadPlus m => StateT s m a
    mzero = StateT $ \_ -> mzero
    mplus :: MonadPlus m => StateT s m a -> StateT s m a -> StateT s m a
    stsma1 `mplus` stsma2 = 
        StateT $ 
            \s -> runStateT stsma1 s `mplus` runStateT stsma2 s
instance MonadPlus m => Alternative (StateT s m) where
    empty :: MonadPlus m => StateT s m a
    empty = mzero
    (<|>) :: MonadPlus m => StateT s m a -> StateT s m a -> StateT s m a
    (<|>) = mplus


