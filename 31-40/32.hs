myGCD :: Int -> Int -> Int
myGCD x y 
 | x < 0 = myGCD (abs x) y
 | y < 0 = myGCD x (abs y)
 | x < y = myGCD y x
 | x `mod` y == 0 = y
 | otherwise = myGCD y (x-y)