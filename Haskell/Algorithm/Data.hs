{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monoid-instances #-}

module Algorithm.Data where
import Control.Monad

newtype Writer w a =
    Writer {
        runWriter :: (a, w)
        }
    deriving (Show)

instance (Monoid w) => Monad (Writer w) where
    return :: a -> Writer w a
    return x = Writer (x, mempty)
    (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    vx >>= f =
        let
            (x,v) = runWriter vx
            (y,w) = runWriter $ f x
        in
            Writer (y, v `mappend` w)

instance (Monoid w) => Applicative (Writer w) where
    pure :: a -> Writer w a
    pure = return
    (<*>) :: Monoid w => Writer w (a -> b) -> Writer w a -> Writer w b
    (<*>) = ap

instance (Monoid w) => Functor (Writer w) where
    fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap = liftM

instance (Monoid w, Monoid a) => Monoid (Writer w a) where
-- instance (Ord a) => Monoid (Writer [a] [a]) where
    mempty :: (Monoid w, Monoid a) => Writer w a
    -- mempty :: (Ord a) => Writer [a] [a]
    mempty = Writer (mempty,mempty)
    mappend :: (Monoid w, Monoid a) => Writer w a -> Writer w a -> Writer w a 
    mappend (Writer (x,v)) (Writer (y,w)) = 
        Writer (x `mappend` y, v `mappend` w)
    -- mappend :: (Ord a) => Writer [a] [a] -> Writer [a] [a] -> Writer [a] [a]
    -- mappend (Writer (x:xs,y:ys)) (Writer (z:zs,w:ws)) = 

instance (Monoid w, Monoid a) => Semigroup (Writer w a) where
        (<>) :: (Monoid w, Monoid a) => Writer w a -> Writer w a -> Writer w a 
        (<>) = mappend

-- instance (Ord a) => Semigroup (Writer [a] [a]) where
--     (<>) :: (Ord a) => Writer [a] [a] -> Writer [a] [a] -> Writer [a] [a]
--     (<>) = mappend

    