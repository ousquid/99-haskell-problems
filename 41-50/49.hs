import Numeric
import Data.Bits
import Data.Char
import Text.Printf
-- gray 3

dec2bin :: Int -> String
dec2bin x = showIntAtBase 2 intToDigit x ""

zeropad :: Int -> String -> String
zeropad n s = printf ("%0"++(show n)++"s") s

gray :: Int -> [String]
gray n = [zeropad n (dec2bin ((shiftR d 1) `xor` d)) | d <- [0..(2^n-1)]] 

gray2 :: Integral a => a -> [String]
gray2 0 = [""]
gray2 n = foldr (\s acc -> ("0" ++ s):("1" ++ s):acc) [] $ gray2 (n-1)