import Data.Char
import Data.List.Split

isValidChar x = (isDigit x) || (isAlpha x) || (x == '-')

identifier :: String -> Bool
identifier str
 | isDigit $ head str = False
 | not $ and $ map isValidChar str = False
 | otherwise = (length $ filter (=="") $ splitOn "-" str) == 0