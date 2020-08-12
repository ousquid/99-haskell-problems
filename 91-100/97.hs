import Data.List
import Debug.Trace

q :: [[Int]]
q = [[0,0,4,8,0,0,0,1,7],
     [6,7,0,9,0,0,0,0,0],
     [5,0,8,0,3,0,0,0,4],
     [3,0,0,7,4,0,1,0,0],
     [0,6,9,0,0,0,7,8,0],
     [0,0,1,0,6,9,0,0,5],
     [1,0,0,0,8,0,3,0,6],
     [0,0,0,0,0,6,0,9,1],
     [2,4,0,0,0,1,5,0,0]]
     
sudoku :: [[Int]] -> [[Int]]
sudoku field = head $ sudoku' field 0

sudoku' :: [[Int]] -> Int -> [[[Int]]]
sudoku' field idx
  | idx >= 81 = [field]
  | field !! ri !! ci /= 0 = sudoku' field (idx+1)
  | otherwise = concat [ sudoku' newField (idx+1) | n <- [1..9], let newField = update field idx n, isValid newField idx ]
  where ri = indexToRow idx
        ci = indexToColumn idx
        
update :: [[Int]] -> Int -> Int -> [[Int]]
update field idx n = take ri field ++ [newRow] ++ drop (ri+1) field
    where ri = indexToRow idx
          ci = indexToColumn idx
          targetRow = field !! ri
          newRow = take ci targetRow ++ [n] ++ drop (ci+1) targetRow

indexToRow :: Int -> Int
indexToRow idx = idx `div` 9
indexToColumn :: Int -> Int
indexToColumn idx = idx `mod` 9

isValid :: [[Int]] -> Int -> Bool
isValid field idx = isValidRow field idx && isValidColumn field idx && isValidSquare field idx

isValidRow :: [[Int]] -> Int -> Bool
isValidRow field idx = isNotDup $ field !! indexToRow idx

isValidColumn :: [[Int]] -> Int -> Bool
isValidColumn field idx = isNotDup $ map (!! indexToColumn idx) field

isValidSquare :: [[Int]] -> Int -> Bool
isValidSquare field idx = isNotDup [ field !! r !! c | r <- rowRange, c <- columnRange ]
    where rowRange = take 3 [((indexToRow idx) `div` 3 * 3) ..]
          columnRange = take 3 [((indexToColumn idx) `div` 3 * 3) ..]

isNotDup :: [Int] -> Bool
isNotDup = all (==1) . map length . group . sort . filter (/=0)
