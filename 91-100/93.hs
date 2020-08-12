import Data.Ratio
import Data.Maybe
import Control.Applicative
import Control.Monad

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty


generateAllTreeShape n = generateAllTreeShape' (n-1)
generateAllTreeShape' :: Int -> [Tree String]
generateAllTreeShape' 0 = [Empty]
generateAllTreeShape' n = [Branch " " left right | leftNum <- [0..n-1], let rightNum = n - leftNum - 1, left <- generateAllTreeShape' leftNum, right <- generateAllTreeShape' rightNum]


generateAllOpPattern :: Tree String -> [Tree String]
generateAllOpPattern (Branch a left right) = [Branch "=" newLeft newRight | newLeft <- generateAllOpPattern' left, newRight <- generateAllOpPattern' right]
generateAllOpPattern' Empty = [Empty]
generateAllOpPattern' (Branch a left right) = [Branch op newLeft newRight | newLeft <- generateAllOpPattern' left, newRight <- generateAllOpPattern' right, op <- ["+", "-", "*", "/"]]

mapNumbersToTree :: [Int] -> Tree String -> Tree String
mapNumbersToTree nums tree = snd $ mapNumbersToTree' tree nums

mapNumbersToTree' :: Tree String -> [Int] -> ([Int], Tree String)
mapNumbersToTree' Empty (x:xs) = (xs, Branch (show x) Empty Empty)
mapNumbersToTree' (Branch a left right) (x:xs) = (xsAfterRight, newTree)
    where (xsAfterLeft, newLeft) = mapNumbersToTree' left (x:xs)
          (xsAfterRight, newRight) = mapNumbersToTree' right xsAfterLeft
          newTree = (Branch a newLeft newRight)
          
evalTree :: Tree String -> Maybe (Ratio Int)
evalTree Empty = Just 0
evalTree (Branch num Empty Empty) = Just (read num % 1) :: Maybe (Ratio Int)
evalTree (Branch op left right)
 | op == "+" =  (+) <$> leftVal <*> rightVal
 | op == "-" =  (-) <$> leftVal <*> rightVal
 | op == "*" =  (*) <$> leftVal <*> rightVal
 | op == "/" = case rightVal of
                 Just val -> if val == 0 then Nothing else (/) <$> leftVal <*> rightVal
                 Nothing -> Nothing
    where leftVal = evalTree left
          rightVal = evalTree right
          
isTreeCorrect :: Tree String -> Bool
isTreeCorrect (Branch _ left right) =
    fromMaybe False $ (==) <$> leftVal <*> rightVal
    where leftVal = evalTree left
          rightVal = evalTree right

-- TODO: treeToString, main
treeToString :: Tree String -> String
treeToString (Branch op left right) = (treeToString' left) ++ " " ++ op ++ " " ++ (treeToString' right)

treeToString' :: Tree String -> String
treeToString' (Branch num Empty Empty) = num
treeToString' (Branch op left@(Branch leftOp _ _) right@(Branch rightOp _ _)) = leftString ++ op ++ rightString
    where leftString = if (op `elem` ["*", "/"]) && (leftOp `elem` ["+", "-"]) then "(" ++ leftContent ++ ")" else leftContent
          leftContent = treeToString' left
          rightString = if (op `elem` ["*", "/"]) && (rightOp `elem` ["+", "-"]) || op == "-" && rightOp `elem` ["+", "-"] then "(" ++ rightContent ++ ")" else rightContent
          rightContent = treeToString' right
    
puzzle :: [Int] -> [String]
puzzle xs = map treeToString $ filter isTreeCorrect allTrees
    where allTrees = map (mapNumbersToTree xs) $ concatMap generateAllOpPattern $ generateAllTreeShape $ length xs