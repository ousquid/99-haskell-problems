import Control.Monad
import Data.List

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
data Which = L | R deriving (Show, Eq, Ord)

cbalTree :: Int -> [Tree String]
cbalTree 1 = [Branch "x" Empty Empty]
cbalTree 2 = [Branch "x" (Branch "x" Empty Empty) Empty, Branch "x" Empty (Branch "x" Empty Empty)]
cbalTree n = cbalTree' (n - bts) btd (Branch "x" parent parent)
    where
      bts = 2^btd - 1
      btd = baseTreeDepth n
      parent = head $ cbalTree ((bts+1) `div` 2 - 1)

cbalTree' :: Int -> Int -> Tree String -> [Tree String]
cbalTree' 0 _ tree = [tree]
cbalTree' n depth tree = [ appendN r tree | r <- combinations n (route depth) ]

-- baseTreeSize :: Int -> Int
-- baseTreeSize x = last $ takeWhile (<= x) [2^n-1 | n <- [1,2..]]

baseTreeDepth :: Int -> Int
baseTreeDepth n = floor $ logBase 2 (fromIntegral (n+1))

appendN :: [[Which]] -> Tree String -> Tree String
appendN routes tree = foldl append tree routes

append :: Tree String -> [Which] -> Tree String
append tree [] = Branch "x" Empty Empty
append (Branch a left right) (L:route) = Branch "x" (append left route) right
append (Branch a left right) (R:route) = Branch "x" left (append right route)

route :: Int -> [[Which]]
route d = replicateM d [L, R]


combinations :: Ord a => Int -> [a] -> [[a]]
combinations 1 xs = [[x] | x <- xs]
combinations n xs = nub (map sort permu)
   where permu = [[x] ++ c | x <- xs, c <- (combinations (n-1) (delete x xs))]


-- [Branch "x" 
--   (Branch "x" (Branch "x" Empty Empty) Empty)
--   (Branch "x" Empty Empty),
-- Branch "x" 
--   (Branch "x" Empty (Branch "x" Empty Empty)) 
--   (Branch "x" Empty Empty),
-- Branch "x" 
--   (Branch "x" Empty Empty)
--   (Branch "x" (Branch "x" Empty Empty) Empty),
-- Branch "x" 
--   (Branch "x" Empty Empty)
--   (Branch "x" Empty (Branch "x" Empty Empty))]  
--      x
--   x   x
--   o o o o
--  LL, LR, RL, RR
  
--   1 1
--   2   2
--   3     3
--     4 4
--     5   5
--       6 6



-- 1 = 1 (log_2 2 = 1)
-- 2 = 1 
-- 3 = 2 (log_2 4 = 2)
-- 4 = 2 
-- ...
-- 6 = 2
-- 7 = 3 (log_2 8 = 3)

-- 2^1 - 1 = 1 -> 1
-- 2～3 -> cbalTree 1 + ???
-- 2^2 - 1 = 3 -> 2
-- 4～7 -> cbalTree 3 + ???
-- 2^3 - 1 = 7 -> 3
-- 8～15 -> cbalTree 7 + ???
-- 2^4 - 1 = 15 -> 4

-- 15

-- cbalTree 1

--     x
    
-- cbalTree 3

--     x
--   x   x
  
-- cbalTree 7

--     x
--   x   x
--  x x x x
 
--  =
 
--     x
--   3   3