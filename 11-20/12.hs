import Data.List

data SM a = Single a | Multiple Int a deriving Show

decodeModified :: [SM a] -> [a]
decodeModified = concatMap smToList 
    where
        smToList (Single s) = [s] 
        smToList (Multiple x m) = replicate x m