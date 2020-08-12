import Data.Tree
import Data.List


huffman :: [(Char, Int)] -> [(Char, String)]
huffman xs = sort (huffman' (map (\(c, f) -> (f, [(c, "")])) xs))

huffman' :: [(Int, [(Char, String)])] -> [(Char, String)]
huffman' (x:[]) = snd x
huffman' xs = let sortedXs = sort xs
                  first = sortedXs !! 0
                  second = sortedXs !! 1
                  remain = drop 2 sortedXs
                  newFreq = fst first + fst second
                  newList = (map (\(c, str) -> (c, "0"++str)) (snd first)) ++ (map (\(c, str) -> (c, "1"++str)) (snd second))
                  newNode = (newFreq, newList)
              in huffman' (newNode:remain)
    
-- xs :: [('a', 45)..]
-- xs :: [(45, [('a',"0"), ('b',"1")]), (6, [('c',"")])]
    
    