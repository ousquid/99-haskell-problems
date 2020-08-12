import Data.List

data SM a = Single a | Multiple Int a deriving Show

encodeModified :: Eq a => [a] -> [SM a]
encodeModified = map sorm . group
    where sorm x
            | length x == 1 = Single (head x) 
            | otherwise     = Multiple (length x) (head x)