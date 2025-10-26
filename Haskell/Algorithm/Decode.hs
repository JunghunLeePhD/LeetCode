module Algorithm.Decode where
import Data.Char (digitToInt)
import Algorithm.Data ( Writer(..) )
import Control.Monad ( (>=>) )

type Code = String
type Key = Int
type Tape = String

repeatN :: Int -> [a] -> [a]
repeatN n xs
    | n <= 0 = []
    | even n =
        repeatN (n `div` 2) xs ++
        repeatN (n `div` 2) xs
    | odd n = xs ++ repeatN (n-1) xs

tapeDecodeOnce :: Writer Tape Code -> Writer Tape Code 
tapeDecodeOnce (Writer ([],tape)) = Writer ([],tape)
tapeDecodeOnce writer@(Writer (x:xs,tape))
    | x `elem` ['0'..'9'] =
        Writer (xs, tape ++ repeatN (digitToInt x - 1) tape)
    | x `elem` ['a'..'z'] = 
        writer >>= (copy >=> delete)
    | x `elem` ['A'..'Z'] = 
        writer >>= (copy >=> delete)
    | otherwise = writer >>= delete
    where
        copy :: Code -> Writer Tape Code 
        copy [] = return []
        copy (x:xs) = Writer (x:xs,[x])
        delete :: Code -> Writer Tape Code 
        delete [] = return []
        delete (_:xs) = return xs

tapeDecodeN :: Int -> Writer Tape Code -> Writer Tape Code
tapeDecodeN n 
    | n <= 0 = id
    | even n = tapeDecodeN (n `div` 2) . tapeDecodeN (n `div` 2) 
    | odd n = tapeDecodeOnce . tapeDecodeN (n-1) 

tapeDecode :: Code -> Tape
tapeDecode code = snd . runWriter . tapeDecodeN (length code) . return $ code

tapeDecodeLookUp :: Code -> Key -> Char
tapeDecodeLookUp code key = tapeDecode code !! key

list1 :: String
list1 = "y959q969u3hb22odq595"
key1 :: Int
key1 = 222280369
test1 :: Char
test1 = tapeDecodeLookUp list1 key1

list2 :: String
list2 = "cpmxv8ewnfk3xxcilcmm68d2ygc88daomywc3imncfjgtwj8nrxjtwhiem5nzqnicxzo248g52y72v3yujqpvqcssrofd99lkovg"
key2 :: Int
key2 = 480551547
test2 :: Char
test2 = tapeDecodeLookUp list2 key2

-- Memory problem:
-- Possible solution: y959 -> y*(9 * 5 * 9) -> y*(405) -> y + y*(404)
-- s = "y959q969u3hb22odq595"
-- k = 222280369