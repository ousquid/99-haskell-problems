import Data.List.Split
import Data.List

data Direction = Vertical | Horizontal deriving (Show, Eq)
-- (row, column, Direction, length)
data Frame = Frame (Int, Int, Direction, Int) deriving Show

parseWords :: String -> [String]
parseWords input = splitOn "\n" input

parseFramework :: String -> [Frame]
parseFramework input = verticals ++ horizontals
    where rows = splitOn "\n" input
          horizontals = [ Frame (row, col, Horizontal, len) | (s, row) <- zip rows [0..], let cs = getChunks s, (col, len) <- cs ]
          cols = transpose rows
          verticals = [ Frame (row, col, Vertical, len) | (s, col) <- zip cols [0..], let cs = getChunks s, (row, len) <- cs ]

getChunks :: String -> [(Int, Int)]  -- (start, length)
getChunks line = map (\x -> (snd $ head x, length x)) dotList
    where dotList = filter (\x -> (fst $ head x) == '.') $ filter (\x -> length x > 1) $ groupBy (\x y -> fst x == fst y) $ zip line [0..]
          

isValidFramework :: [(String, Frame)] -> Bool
isValidFramework framework = isValidLength && isValidPlacement
    where isValidLength = all (\(word, Frame (_,_,_,frameLen)) -> length word == frameLen) framework
          isValidPlacement = isValidPlacement' (emptyBoard (map snd framework)) framework
          isValidPlacement' _ [] = True
          isValidPlacement' board ((word, frame):framework) = isValid && isValidPlacement' newBoard framework
            where (newBoard, isValid) = updateBoard board word frame 
-- 98.hsを流用しない (transpose を使おう)

emptyBoard :: [Frame] -> [[Char]]
emptyBoard frames = take maxRow $ repeat emptyRow
    where emptyRow = take maxCol $ repeat '.'
          maxRow = maximum [row + len | Frame (row, col, direction, len) <- frames, direction == Vertical]
          maxCol = maximum [col + len | Frame (row, col, direction, len) <- frames, direction == Horizontal]

updateBoard :: [[Char]] -> String -> Frame -> ([[Char]], Bool)
updateBoard board word (Frame (row, col, direction, len))
 | direction == Vertical   = let (newBoard, isValid) = updateBoard (transpose board) word (Frame (col, row, Horizontal, len)) in (transpose newBoard, isValid)
 | direction == Horizontal = (take row board ++ [newRow] ++ drop (row + 1) board, isValid)
    where newRow = take col (board!!row) ++ word ++ drop (col + (length word)) (board!!row)
          isValid = all (\(orig, new) -> not (orig /= '.' && orig /= new)) $ zip (board!!row) newRow
 

readCrossword :: String -> ([String], [Frame])
readCrossword input = (parseWords (strs!!0), parseFramework (strs!!1))
    where strs = splitOn "\n\n" input

solve ::  ([String], [Frame]) -> [[(String, Frame)]]
solve (words, frames) = filter isValidFramework [framework | pattern <- permutations words, let framework = zip pattern frames]