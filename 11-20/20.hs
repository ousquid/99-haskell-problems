removeAt :: Int -> [a] -> (a, [a])
removeAt n x = (x !! (n-1), take (n-1) x ++ drop n x)