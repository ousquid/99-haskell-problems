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

--table :: (Bool -> Bool -> Bool) -> IO ()
table f = do mapM_ putStrLn [show x ++ " " ++ show y ++ " " ++ show (f x y)| x <- [True, False], y <- [True, False]]

table' f = do 
  mapM_ putStrLn $ do
    x <- [True, False]
    y <- [True, False]
  --putStrLn $ show x
    --return (x, y, f x y)
    return $ show x ++ " " ++ show y ++ " " ++ show (f x y)
  --z <- return (x, y)
  --putStrLn $ show z
  -- a <- putStr $ f x y
  --let x = 1
  --print 2
  --print 3
  --return [(x, y)]
  
