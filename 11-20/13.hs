data SM a = Single a | Multiple Int a deriving Show

encodeDirect :: Eq a => [a] -> [SM a]
encodeDirect = reverse . foldl encodeHelper []
    where
        encodeHelper [] x = [Single x]
        encodeHelper ((Single e):xs) x 
            | e == x = (Multiple 2 e):xs
            | otherwise = (Single x):(Single e):xs
        encodeHelper ((Multiple i e):xs) x
            | e == x = (Multiple (i+1) e):xs
            | otherwise = (Single x):(Multiple i e):xs
