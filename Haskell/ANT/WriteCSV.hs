module MPS23.WriteCSV where

type Path = String

writeOneLine :: (Show a, Show b) => (a, b) -> String
writeOneLine (k,v) = show k ++ "," ++ show v ++ "\n"
writeContent :: (Show a, Show b) => [(a, b)] -> String
writeContent kvs = kvs >>= writeOneLine
writeCSV :: (Show a, Show b) => Path -> [(a, b)] -> IO ()
writeCSV path = writeFile path . writeContent



