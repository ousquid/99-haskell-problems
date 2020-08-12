import Control.Monad
import Data.List
import Data.List.Split
import Debug.Trace

-- mapM_ (putStrLn . show) $ picross [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]] [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]
-- 
--  1
-- 414
-- xxx3
-- x x11
-- x x11
-- xxx3
--
-- rowNum: 4
-- colNum: 3

sampleRows :: [[Int]]
sampleRows = [[3],[1,1],[1,1],[3]]
sampleCols :: [[Int]]
sampleCols = [[4],[1,1],[4]]
sampleField :: [Int]
sampleField = take ((length sampleRows)*(length sampleCols)) [0..] 

picross :: [[Int]] -> [[Int]] -> [[Int]]
picross solidRows solidCols = chunksOf colNum $ head $ picross' solidRows solidCols initField
    where rowNum = length solidRows
          colNum = length solidCols
          initField = replicate (rowNum*colNum) (-1) 

-- -1: NotYet, 0: Unassing, 1: Assign
picross' :: [[Int]] -> [[Int]] -> [Int] -> [[Int]]
picross' solidRows solidCols field
    | all (/=(-1)) field = [field] 
    | otherwise = concat [picross' solidRows solidCols fi | (_,fi) <- allCandiTups]
    where rowNum = length solidRows
          colNum = length solidCols
          
          -- [(candiNum, field)]
          rowCandiTups = [(length cands, updateRow field colNum rowIdx cand) | rowIdx <- [0..(rowNum-1)], let row = getRow field colNum rowIdx, any (==(-1)) row, let cands = getCandidatedLines row (solidRows !! rowIdx), cand <- cands] 
          colCandiTups = [(length cands, updateColumn field colNum colIdx cand) | colIdx <- [0..(colNum-1)], let col = getColumn field colNum colIdx, any (==(-1)) col, let cands = getCandidatedLines col (solidCols !! colIdx), cand <- cands] 
          allCandiTups = sort $ (rowCandiTups++colCandiTups)
          

indexToRow :: Int -> Int -> Int
indexToRow rowNum idx = idx `div` rowNum

indexToColumn :: Int -> Int -> Int
indexToColumn colNum idx = idx `mod` colNum

getRow :: [Int] -> Int -> Int -> [Int]
getRow field colNum rowIdx = take colNum $ drop (rowIdx*colNum) field

getColumn :: [Int] -> Int -> Int -> [Int]
getColumn field colNum colIdx = [cell | (cell, refColIdx) <- (zip field $cycle [0..(colNum-1)]), colIdx == refColIdx]

getCandidatedLines :: [Int] -> [Int] -> [[Int]]
getCandidatedLines line solid = filter (\x -> isSameSolid x && isSameAssign line x) $ replicateM (length line) [0, 1]
    where isSameSolid tarline = (map length $ filter (\x -> (head x)==1) $ group tarline) == solid
          isSameAssign refline tarline = not (any (\(r,t) -> (r==1 && t/=1) || (r==0 && t/=0)) $ zip refline tarline)
          
updateRow :: [Int] -> Int -> Int -> [Int] -> [Int]
updateRow field colNum rowIdx row = (take (rowIdx*colNum) field) ++ row ++ (drop ((rowIdx+1)*colNum) field)
             
updateColumn :: [Int] -> Int -> Int -> [Int] -> [Int]
updateColumn field colNum colIdx col = concat [(take colIdx colChunk) ++ [colItem] ++ (drop (colIdx+1) colChunk) | (colItem, colChunk) <- zip col $ chunksOf colNum field]
