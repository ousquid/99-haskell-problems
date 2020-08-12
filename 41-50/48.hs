import Data.List
import Control.Monad (replicateM)

infixl 4 `or'`
infixl 4 `nor'`
infixl 5 `xor'`
infixl 6 `and'`
infixl 6 `nand'`
--infixl 3 `equ'` 
infixl 7 `equ'`

and' x y = and [x, y]
or' x y = or [x, y]
nand' x y = not $ and' x y
nor' x y = not $ or' x y
xor' x y 
 | x==y = False
 | otherwise = True
impl' x y
 | (x==True) && (y==False) = False
 | otherwise = True
equ' x y = x == y

table f = do mapM_ putStrLn [show x ++ " " ++ show y ++ " " ++ show (f x y)| x <- [True, False], y <- [True, False]]

--tablen :: Integer -> ([Bool] -> Bool) -> IO ()
tablen n f = do mapM_ putStrLn [intercalate " " $ map show (x ++ [f x]) | x <- replicateM n [True, False]]