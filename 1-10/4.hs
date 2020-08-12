myLength :: [a] -> Int
myLength = foldl (const . (+1)) 0