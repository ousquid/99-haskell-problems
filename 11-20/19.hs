oneRotate :: [a] -> [a]
oneRotate xs = tail xs ++ [head xs]

rotate :: [a] -> Int -> [a]
rotate xs n = let len = length xs in iterate oneRotate xs !! ((n `mod` len) + len)