import Data.List
import Data.List.Split

data Tree a = Node a [Tree a]
        deriving (Eq, Show)
        
tree1 = Node 'a' []

tree2 = Node 'a' [Node 'b' []]

tree3 = Node 'a' [Node 'b' [Node 'c' []]]

tree4 = Node 'b' [Node 'd' [], Node 'e' []]

tree5 = Node 'a' [
                Node 'f' [Node 'g' []],
                Node 'c' [],
                Node 'b' [Node 'd' [], Node 'e' []]
                ]
        
nnodes :: Tree a -> Int
nnodes (Node _ []) = 1
nnodes (Node _ xs) = 1 + (sum $ map nnodes xs)


-- stringToTree :: String -> Tree Char
-- stringToTree str = fst $ ds2tree' str

indent :: Char -> Int
indent c = case c of '^' -> -1
                     otherwise -> 1

indentLevel :: String -> Int
indentLevel s = sum $ map indent s

zeroIndentIndices :: String -> [Int]
zeroIndentIndices s = findIndices (==0) $ map indentLevel $ tail $ inits s

stringToTree :: String -> Tree Char
stringToTree (c:xs) = Node c children
    where indices = zeroIndentIndices $ init xs
          split = splitPlaces (zipWith (-) indices (-1:indices)) xs
          children = map stringToTree split

treeToString :: Tree Char -> String
treeToString (Node a l) =  [a] ++ concatMap treeToString l ++ "^"





